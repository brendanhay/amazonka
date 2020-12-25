{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.RetentionLockType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.RetentionLockType
  ( RetentionLockType
      ( RetentionLockType',
        RetentionLockTypeCompliance,
        RetentionLockTypeGovernance,
        RetentionLockTypeNone,
        fromRetentionLockType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RetentionLockType = RetentionLockType'
  { fromRetentionLockType ::
      Core.Text
  }
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

pattern RetentionLockTypeCompliance :: RetentionLockType
pattern RetentionLockTypeCompliance = RetentionLockType' "COMPLIANCE"

pattern RetentionLockTypeGovernance :: RetentionLockType
pattern RetentionLockTypeGovernance = RetentionLockType' "GOVERNANCE"

pattern RetentionLockTypeNone :: RetentionLockType
pattern RetentionLockTypeNone = RetentionLockType' "NONE"

{-# COMPLETE
  RetentionLockTypeCompliance,
  RetentionLockTypeGovernance,
  RetentionLockTypeNone,
  RetentionLockType'
  #-}
