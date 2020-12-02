{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsWebdavHTTPTransferMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsWebdavHTTPTransferMode where

import Network.AWS.Prelude

-- | Hls Webdav Http Transfer Mode
data HlsWebdavHTTPTransferMode
  = WebdavChunked
  | WebdavNonChunked
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

instance FromText HlsWebdavHTTPTransferMode where
  parser =
    takeLowerText >>= \case
      "chunked" -> pure WebdavChunked
      "non_chunked" -> pure WebdavNonChunked
      e ->
        fromTextError $
          "Failure parsing HlsWebdavHTTPTransferMode from value: '" <> e
            <> "'. Accepted values: chunked, non_chunked"

instance ToText HlsWebdavHTTPTransferMode where
  toText = \case
    WebdavChunked -> "CHUNKED"
    WebdavNonChunked -> "NON_CHUNKED"

instance Hashable HlsWebdavHTTPTransferMode

instance NFData HlsWebdavHTTPTransferMode

instance ToByteString HlsWebdavHTTPTransferMode

instance ToQuery HlsWebdavHTTPTransferMode

instance ToHeader HlsWebdavHTTPTransferMode

instance ToJSON HlsWebdavHTTPTransferMode where
  toJSON = toJSONText

instance FromJSON HlsWebdavHTTPTransferMode where
  parseJSON = parseJSONText "HlsWebdavHTTPTransferMode"
