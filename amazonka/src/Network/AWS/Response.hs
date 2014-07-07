-- Module      : Network.AWS.Response
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Response where

import           Control.Applicative
import           Control.Exception            (Exception)
import           Control.Lens                 hiding (Action)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson                   hiding (Error)
import qualified Data.Attoparsec.Text         as AText
import           Data.ByteString              (ByteString)
import           Data.Char
import           Data.Conduit
import           Data.Default
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Time
import           Data.Typeable
import           Network.AWS.Data
import qualified Network.HTTP.Client          as Client
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method

import qualified Crypto.Hash.SHA256         as SHA256
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Int
import           Data.Monoid
import           Data.String
import           Network.HTTP.Client

-- parse :: MonadResource m
--       => ClientResponse (ResumableSource m ByteString)
--       -> m (Either (Er (Sv a)) (Rs a))
-- parse rs
--     |

-- Make status code exceptions inhabitants of the AWSError sum?

-- s3Response :: a
--            -> S3Response
--            -> AWS (Either AWSError (Either S3ErrorResponse S3Response))
-- s3Response _ rs
--     | code >= 200 && code < 300 = return . Right $ Right rs
--     | otherwise = do
--         lbs <- responseBody rs $$+- Conduit.sinkLbs
--         if LBS.null lbs
--             then return . Right . Left $
--                 S3ErrorResponse (Text.pack $ show rs) "" "Empty Response"
--             else do
--                 whenDebug . liftIO $ LBS.putStrLn $ "[ResponseBody]\n" <> lbs
--                 return . either Left (Right . Left) . parse $ LBS.toStrict lbs
--   where
--     parse :: ByteString -> Either AWSError S3ErrorResponse
--     parse = fmapL toError . fromXML

--     -- plain      = Right rs
--     -- badRequest = Left $ S3ErrorResponse "Bad Request." (Just 400)
--     -- forbidden  = Left $ S3ErrorResponse "Forbidden."   (Just 403)
--     -- notFound   = Left $ S3ErrorResponse "Not Found."   (Just 404)

--     code = statusCode (responseStatus rs)
