{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ContentEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ContentEncoding where

import Network.AWS.Prelude

data ContentEncoding
  = CEGzip
  | CENone
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

instance FromText ContentEncoding where
  parser =
    takeLowerText >>= \case
      "gzip" -> pure CEGzip
      "none" -> pure CENone
      e ->
        fromTextError $
          "Failure parsing ContentEncoding from value: '" <> e
            <> "'. Accepted values: gzip, none"

instance ToText ContentEncoding where
  toText = \case
    CEGzip -> "GZIP"
    CENone -> "NONE"

instance Hashable ContentEncoding

instance NFData ContentEncoding

instance ToByteString ContentEncoding

instance ToQuery ContentEncoding

instance ToHeader ContentEncoding

instance ToJSON ContentEncoding where
  toJSON = toJSONText

instance FromJSON ContentEncoding where
  parseJSON = parseJSONText "ContentEncoding"
