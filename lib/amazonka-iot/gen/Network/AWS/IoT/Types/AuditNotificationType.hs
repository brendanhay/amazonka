{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditNotificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditNotificationType where

import Network.AWS.Prelude

data AuditNotificationType = ANTSNS
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

instance FromText AuditNotificationType where
  parser =
    takeLowerText >>= \case
      "sns" -> pure ANTSNS
      e ->
        fromTextError $
          "Failure parsing AuditNotificationType from value: '" <> e
            <> "'. Accepted values: sns"

instance ToText AuditNotificationType where
  toText = \case
    ANTSNS -> "SNS"

instance Hashable AuditNotificationType

instance NFData AuditNotificationType

instance ToByteString AuditNotificationType

instance ToQuery AuditNotificationType

instance ToHeader AuditNotificationType

instance ToJSON AuditNotificationType where
  toJSON = toJSONText

instance FromJSON AuditNotificationType where
  parseJSON = parseJSONText "AuditNotificationType"
