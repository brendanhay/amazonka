{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyType
  ( PolicyType
      ( PolicyType',
        PolicyTypeServiceControlPolicy,
        PolicyTypeTagPolicy,
        PolicyTypeBackupPolicy,
        PolicyTypeAiservicesOptOutPolicy,
        fromPolicyType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PolicyType = PolicyType' {fromPolicyType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern PolicyTypeServiceControlPolicy :: PolicyType
pattern PolicyTypeServiceControlPolicy = PolicyType' "SERVICE_CONTROL_POLICY"

pattern PolicyTypeTagPolicy :: PolicyType
pattern PolicyTypeTagPolicy = PolicyType' "TAG_POLICY"

pattern PolicyTypeBackupPolicy :: PolicyType
pattern PolicyTypeBackupPolicy = PolicyType' "BACKUP_POLICY"

pattern PolicyTypeAiservicesOptOutPolicy :: PolicyType
pattern PolicyTypeAiservicesOptOutPolicy = PolicyType' "AISERVICES_OPT_OUT_POLICY"

{-# COMPLETE
  PolicyTypeServiceControlPolicy,
  PolicyTypeTagPolicy,
  PolicyTypeBackupPolicy,
  PolicyTypeAiservicesOptOutPolicy,
  PolicyType'
  #-}
