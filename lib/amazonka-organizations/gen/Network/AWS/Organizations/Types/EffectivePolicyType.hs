{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EffectivePolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.EffectivePolicyType
  ( EffectivePolicyType
      ( EffectivePolicyType',
        EPTAiservicesOptOutPolicy,
        EPTBackupPolicy,
        EPTTagPolicy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EffectivePolicyType = EffectivePolicyType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern EPTAiservicesOptOutPolicy :: EffectivePolicyType
pattern EPTAiservicesOptOutPolicy = EffectivePolicyType' "AISERVICES_OPT_OUT_POLICY"

pattern EPTBackupPolicy :: EffectivePolicyType
pattern EPTBackupPolicy = EffectivePolicyType' "BACKUP_POLICY"

pattern EPTTagPolicy :: EffectivePolicyType
pattern EPTTagPolicy = EffectivePolicyType' "TAG_POLICY"

{-# COMPLETE
  EPTAiservicesOptOutPolicy,
  EPTBackupPolicy,
  EPTTagPolicy,
  EffectivePolicyType'
  #-}
