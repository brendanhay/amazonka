{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SNSActionEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SNSActionEncoding where

import Network.AWS.Prelude

data SNSActionEncoding
  = BASE64
  | Utf8
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

instance FromText SNSActionEncoding where
  parser =
    takeLowerText >>= \case
      "base64" -> pure BASE64
      "utf-8" -> pure Utf8
      e ->
        fromTextError $
          "Failure parsing SNSActionEncoding from value: '" <> e
            <> "'. Accepted values: base64, utf-8"

instance ToText SNSActionEncoding where
  toText = \case
    BASE64 -> "Base64"
    Utf8 -> "UTF-8"

instance Hashable SNSActionEncoding

instance NFData SNSActionEncoding

instance ToByteString SNSActionEncoding

instance ToQuery SNSActionEncoding

instance ToHeader SNSActionEncoding

instance FromXML SNSActionEncoding where
  parseXML = parseXMLText "SNSActionEncoding"
