{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogEncryptionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogEncryptionMode where

import Network.AWS.Prelude

data CatalogEncryptionMode
  = CEMDisabled
  | CEMSseKMS
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

instance FromText CatalogEncryptionMode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure CEMDisabled
      "sse-kms" -> pure CEMSseKMS
      e ->
        fromTextError $
          "Failure parsing CatalogEncryptionMode from value: '" <> e
            <> "'. Accepted values: disabled, sse-kms"

instance ToText CatalogEncryptionMode where
  toText = \case
    CEMDisabled -> "DISABLED"
    CEMSseKMS -> "SSE-KMS"

instance Hashable CatalogEncryptionMode

instance NFData CatalogEncryptionMode

instance ToByteString CatalogEncryptionMode

instance ToQuery CatalogEncryptionMode

instance ToHeader CatalogEncryptionMode

instance ToJSON CatalogEncryptionMode where
  toJSON = toJSONText

instance FromJSON CatalogEncryptionMode where
  parseJSON = parseJSONText "CatalogEncryptionMode"
