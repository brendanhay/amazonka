{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EffectivePolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.EffectivePolicyType
  ( EffectivePolicyType
    ( EffectivePolicyType'
    , EffectivePolicyTypeTagPolicy
    , EffectivePolicyTypeBackupPolicy
    , EffectivePolicyTypeAiservicesOptOutPolicy
    , fromEffectivePolicyType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EffectivePolicyType = EffectivePolicyType'{fromEffectivePolicyType
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern EffectivePolicyTypeTagPolicy :: EffectivePolicyType
pattern EffectivePolicyTypeTagPolicy = EffectivePolicyType' "TAG_POLICY"

pattern EffectivePolicyTypeBackupPolicy :: EffectivePolicyType
pattern EffectivePolicyTypeBackupPolicy = EffectivePolicyType' "BACKUP_POLICY"

pattern EffectivePolicyTypeAiservicesOptOutPolicy :: EffectivePolicyType
pattern EffectivePolicyTypeAiservicesOptOutPolicy = EffectivePolicyType' "AISERVICES_OPT_OUT_POLICY"

{-# COMPLETE 
  EffectivePolicyTypeTagPolicy,

  EffectivePolicyTypeBackupPolicy,

  EffectivePolicyTypeAiservicesOptOutPolicy,
  EffectivePolicyType'
  #-}
