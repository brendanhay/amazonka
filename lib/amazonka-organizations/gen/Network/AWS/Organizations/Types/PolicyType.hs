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
        AiservicesOptOutPolicy,
        BackupPolicy,
        ServiceControlPolicy,
        TagPolicy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PolicyType = PolicyType' Lude.Text
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

pattern AiservicesOptOutPolicy :: PolicyType
pattern AiservicesOptOutPolicy = PolicyType' "AISERVICES_OPT_OUT_POLICY"

pattern BackupPolicy :: PolicyType
pattern BackupPolicy = PolicyType' "BACKUP_POLICY"

pattern ServiceControlPolicy :: PolicyType
pattern ServiceControlPolicy = PolicyType' "SERVICE_CONTROL_POLICY"

pattern TagPolicy :: PolicyType
pattern TagPolicy = PolicyType' "TAG_POLICY"

{-# COMPLETE
  AiservicesOptOutPolicy,
  BackupPolicy,
  ServiceControlPolicy,
  TagPolicy,
  PolicyType'
  #-}
