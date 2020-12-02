{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CloudWatchEncryptionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CloudWatchEncryptionMode where

import Network.AWS.Prelude

data CloudWatchEncryptionMode
  = CWEMDisabled
  | CWEMSseKMS
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

instance FromText CloudWatchEncryptionMode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure CWEMDisabled
      "sse-kms" -> pure CWEMSseKMS
      e ->
        fromTextError $
          "Failure parsing CloudWatchEncryptionMode from value: '" <> e
            <> "'. Accepted values: disabled, sse-kms"

instance ToText CloudWatchEncryptionMode where
  toText = \case
    CWEMDisabled -> "DISABLED"
    CWEMSseKMS -> "SSE-KMS"

instance Hashable CloudWatchEncryptionMode

instance NFData CloudWatchEncryptionMode

instance ToByteString CloudWatchEncryptionMode

instance ToQuery CloudWatchEncryptionMode

instance ToHeader CloudWatchEncryptionMode

instance ToJSON CloudWatchEncryptionMode where
  toJSON = toJSONText

instance FromJSON CloudWatchEncryptionMode where
  parseJSON = parseJSONText "CloudWatchEncryptionMode"
