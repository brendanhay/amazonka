-- |
-- Module      : Amazonka.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Endpoint
  ( -- * Endpoint
    setEndpoint,
    defaultEndpoint,
  )
where

import Amazonka.Data.ByteString
import Amazonka.Lens ((%~), (.~), field, (^.), setField)
import Amazonka.Prelude
import Amazonka.Types
import qualified Data.CaseInsensitive as CI

-- | A convenience function for overriding the 'Service' 'Endpoint'.
--
-- /See:/ 'serviceEndpoint'.
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
setEndpoint s h p = serviceEndpoint %~ addr
  where
    addr =
      (endpointSecure .~ s)
        . (endpointHost .~ h)
        . (endpointPort .~ p)


-- | Determine the full host address and credential scope
-- within the specified 'Region'.
defaultEndpoint :: Service -> Region -> Endpoint
defaultEndpoint (serviceEndpointPrefix -> p) r = go (CI.mk p)
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

    region h =
      Endpoint
        { endpointHost = h,
          endpointSecure = True,
          endpointPort = 443,
          endpointScope = reg
        }

    global h =
      Endpoint
        { endpointHost = h,
          endpointSecure = True,
          endpointPort = 443,
          endpointScope = "us-east-1"
        }

    reg = toBS r
