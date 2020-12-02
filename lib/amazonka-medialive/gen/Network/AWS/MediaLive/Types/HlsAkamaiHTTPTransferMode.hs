{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsAkamaiHTTPTransferMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsAkamaiHTTPTransferMode where

import Network.AWS.Prelude

-- | Hls Akamai Http Transfer Mode
data HlsAkamaiHTTPTransferMode
  = AkamaiChunked
  | AkamaiNonChunked
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText HlsAkamaiHTTPTransferMode where
  parser =
    takeLowerText >>= \case
      "chunked" -> pure AkamaiChunked
      "non_chunked" -> pure AkamaiNonChunked
      e ->
        fromTextError $
          "Failure parsing HlsAkamaiHTTPTransferMode from value: '" <> e
            <> "'. Accepted values: chunked, non_chunked"

instance ToText HlsAkamaiHTTPTransferMode where
  toText = \case
    AkamaiChunked -> "CHUNKED"
    AkamaiNonChunked -> "NON_CHUNKED"

instance Hashable HlsAkamaiHTTPTransferMode

instance NFData HlsAkamaiHTTPTransferMode

instance ToByteString HlsAkamaiHTTPTransferMode

instance ToQuery HlsAkamaiHTTPTransferMode

instance ToHeader HlsAkamaiHTTPTransferMode

instance ToJSON HlsAkamaiHTTPTransferMode where
  toJSON = toJSONText

instance FromJSON HlsAkamaiHTTPTransferMode where
  parseJSON = parseJSONText "HlsAkamaiHTTPTransferMode"
