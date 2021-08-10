-- |
-- Module      : Network.AWS.Bytes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Bytes
  ( convert,
    encodeBase16,
    decodeBase16,
    encodeBase64,
    decodeBase64,
  )
where

import Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteArray.Encoding as ByteArray.Encoding
import Network.AWS.Prelude

encodeBase16 :: ByteArrayAccess a => a -> ByteString
encodeBase16 = ByteArray.Encoding.convertToBase ByteArray.Encoding.Base16

decodeBase16 :: ByteArrayAccess a => a -> Either String ByteString
decodeBase16 = ByteArray.Encoding.convertFromBase ByteArray.Encoding.Base16

encodeBase64 :: ByteArrayAccess a => a -> ByteString
encodeBase64 = ByteArray.Encoding.convertToBase ByteArray.Encoding.Base64

decodeBase64 :: ByteArrayAccess a => a -> Either String ByteString
decodeBase64 = ByteArray.Encoding.convertFromBase ByteArray.Encoding.Base64
