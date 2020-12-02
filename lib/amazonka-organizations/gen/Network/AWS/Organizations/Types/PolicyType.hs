{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyType where

import Network.AWS.Prelude

data PolicyType
  = AiservicesOptOutPolicy
  | BackupPolicy
  | ServiceControlPolicy
  | TagPolicy
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

instance FromText PolicyType where
  parser =
    takeLowerText >>= \case
      "aiservices_opt_out_policy" -> pure AiservicesOptOutPolicy
      "backup_policy" -> pure BackupPolicy
      "service_control_policy" -> pure ServiceControlPolicy
      "tag_policy" -> pure TagPolicy
      e ->
        fromTextError $
          "Failure parsing PolicyType from value: '" <> e
            <> "'. Accepted values: aiservices_opt_out_policy, backup_policy, service_control_policy, tag_policy"

instance ToText PolicyType where
  toText = \case
    AiservicesOptOutPolicy -> "AISERVICES_OPT_OUT_POLICY"
    BackupPolicy -> "BACKUP_POLICY"
    ServiceControlPolicy -> "SERVICE_CONTROL_POLICY"
    TagPolicy -> "TAG_POLICY"

instance Hashable PolicyType

instance NFData PolicyType

instance ToByteString PolicyType

instance ToQuery PolicyType

instance ToHeader PolicyType

instance ToJSON PolicyType where
  toJSON = toJSONText

instance FromJSON PolicyType where
  parseJSON = parseJSONText "PolicyType"
