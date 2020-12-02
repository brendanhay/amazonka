{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobBookmarksEncryptionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobBookmarksEncryptionMode where

import Network.AWS.Prelude

data JobBookmarksEncryptionMode
  = JBEMCseKMS
  | JBEMDisabled
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

instance FromText JobBookmarksEncryptionMode where
  parser =
    takeLowerText >>= \case
      "cse-kms" -> pure JBEMCseKMS
      "disabled" -> pure JBEMDisabled
      e ->
        fromTextError $
          "Failure parsing JobBookmarksEncryptionMode from value: '" <> e
            <> "'. Accepted values: cse-kms, disabled"

instance ToText JobBookmarksEncryptionMode where
  toText = \case
    JBEMCseKMS -> "CSE-KMS"
    JBEMDisabled -> "DISABLED"

instance Hashable JobBookmarksEncryptionMode

instance NFData JobBookmarksEncryptionMode

instance ToByteString JobBookmarksEncryptionMode

instance ToQuery JobBookmarksEncryptionMode

instance ToHeader JobBookmarksEncryptionMode

instance ToJSON JobBookmarksEncryptionMode where
  toJSON = toJSONText

instance FromJSON JobBookmarksEncryptionMode where
  parseJSON = parseJSONText "JobBookmarksEncryptionMode"
