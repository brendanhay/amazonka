-- |
-- Module      : Amazonka.Endpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Endpoint
  ( -- * Endpoint
    setEndpoint,
    defaultEndpoint,
    customEndpoints,
  )
where

import Amazonka.Data.ByteString
import Amazonka.Data.Text (toText)
import Amazonka.Prelude
import Amazonka.Types
import qualified Amazonka.Types as Service (Service (abbrev))
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified System.Environment as Environment

-- | A convenience function for overriding the 'Service' 'Endpoint'.
--
-- /See:/ 'endpoint'.
setEndpoint ::
  -- | Whether to use HTTPS (ie. SSL).
  Bool ->
  -- | The hostname to connect to.
  ByteString ->
  -- | The port number to connect to.
  Int ->
  -- | The service configuration to override.
  Service ->
  Service
setEndpoint secure host port s@Service {endpoint} =
  s {endpoint = \r -> (endpoint r) {secure, host, port}}

-- | Determine the full host address and credential scope
-- within the specified 'Region'.
defaultEndpoint :: Service -> Region -> Endpoint
defaultEndpoint Service {endpointPrefix = p} r = go (CI.mk p)
  where
    go = \case
      "iam"
        | china -> region "iam.cn-north-1.amazonaws.com.cn"
        | govcloud -> region "iam.us-gov.amazonaws.com"
        | otherwise -> global "iam.amazonaws.com"
      "sdb"
        | virginia -> region "sdb.amazonaws.com"
      "sts"
        | china -> region "sts.cn-north-1.amazonaws.com.cn"
        | govcloud -> region ("sts." <> reg <> ".amazonaws.com")
        | otherwise -> global "sts.amazonaws.com"
      "rds"
        | virginia -> global "rds.amazonaws.com"
      "route53"
        | not china -> global "route53.amazonaws.com"
      "emr"
        | virginia -> global "elasticmapreduce.us-east-1.amazonaws.com"
        | china -> region "elasticmapreduce.cn-north-1.amazonaws.com.cn"
        | frankfurt -> region "elasticmapreduce.eu-central-1.amazonaws.com"
        | otherwise -> region (reg <> ".elasticmapreduce.amazonaws.com")
      "sqs"
        | virginia -> global "queue.amazonaws.com"
        | china -> region (reg <> ".queue.amazonaws.com.cn")
      "importexport"
        | not china -> region "importexport.amazonaws.com"
      "cloudfront"
        | not china -> global "cloudfront.amazonaws.com"
      "waf"
        | not china -> global "waf.amazonaws.com"
      _other
        | china -> region (p <> "." <> reg <> ".amazonaws.com.cn")
        | otherwise -> region (p <> "." <> reg <> ".amazonaws.com")

    virginia = r == NorthVirginia
    frankfurt = r == Frankfurt
    china = r == Beijing
    govcloud = r == GovCloudEast || r == GovCloudWest

    region host =
      Endpoint
        { host,
          basePath = mempty,
          secure = True,
          port = 443,
          scope = reg
        }

    global host =
      Endpoint
        { host,
          basePath = mempty,
          secure = True,
          port = 443,
          scope = "us-east-1"
        }

    reg = toBS r

-- | Retrieve custom endpoints from environment variables:
--
-- * @AWS_ENDPOINT_URL@
-- * @AWS_ENDPOINT_URL_<SERVICE>@
--
-- The latter takes precedence over the former.
customEndpoints :: (MonadIO m) => m (Service -> Service)
customEndpoints = do
  environment <- liftIO Environment.getEnvironment
  let globalUrl = lookup "AWS_ENDPOINT_URL" environment >>= Client.parseUrlThrow
  let serviceUrls = mapMaybe (\(k, v) -> (,v) . map Char.toLower <$> removePrefix "AWS_ENDPOINT_URL_" k) environment
  let override s =
        case lookup (Text.unpack . Text.toLower . toText $ Service.abbrev s) serviceUrls of
          Just x -> setEndpointMaybe (Client.parseUrlThrow x) s
          Nothing -> setEndpointMaybe globalUrl s
  pure override
  where
    removePrefix :: String -> String -> Maybe String
    removePrefix prefix s =
      if prefix `List.isPrefixOf` s
        then Just $ drop (length prefix) s
        else Nothing

    setEndpointMaybe :: Maybe Client.Request -> Service -> Service
    setEndpointMaybe mreq s =
      case mreq of
        Just req -> setEndpoint (Client.secure req) (Client.host req) (Client.port req) s
        Nothing -> s
