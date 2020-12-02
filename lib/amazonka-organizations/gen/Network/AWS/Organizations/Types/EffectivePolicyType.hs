{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EffectivePolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.EffectivePolicyType where

import Network.AWS.Prelude

data EffectivePolicyType
  = EPTAiservicesOptOutPolicy
  | EPTBackupPolicy
  | EPTTagPolicy
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

instance FromText EffectivePolicyType where
  parser =
    takeLowerText >>= \case
      "aiservices_opt_out_policy" -> pure EPTAiservicesOptOutPolicy
      "backup_policy" -> pure EPTBackupPolicy
      "tag_policy" -> pure EPTTagPolicy
      e ->
        fromTextError $
          "Failure parsing EffectivePolicyType from value: '" <> e
            <> "'. Accepted values: aiservices_opt_out_policy, backup_policy, tag_policy"

instance ToText EffectivePolicyType where
  toText = \case
    EPTAiservicesOptOutPolicy -> "AISERVICES_OPT_OUT_POLICY"
    EPTBackupPolicy -> "BACKUP_POLICY"
    EPTTagPolicy -> "TAG_POLICY"

instance Hashable EffectivePolicyType

instance NFData EffectivePolicyType

instance ToByteString EffectivePolicyType

instance ToQuery EffectivePolicyType

instance ToHeader EffectivePolicyType

instance ToJSON EffectivePolicyType where
  toJSON = toJSONText

instance FromJSON EffectivePolicyType where
  parseJSON = parseJSONText "EffectivePolicyType"
