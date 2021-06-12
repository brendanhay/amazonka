{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.RetentionLockType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.RetentionLockType
  ( RetentionLockType
      ( ..,
        RetentionLockType_COMPLIANCE,
        RetentionLockType_GOVERNANCE,
        RetentionLockType_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RetentionLockType = RetentionLockType'
  { fromRetentionLockType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern RetentionLockType_COMPLIANCE :: RetentionLockType
pattern RetentionLockType_COMPLIANCE = RetentionLockType' "COMPLIANCE"

pattern RetentionLockType_GOVERNANCE :: RetentionLockType
pattern RetentionLockType_GOVERNANCE = RetentionLockType' "GOVERNANCE"

pattern RetentionLockType_NONE :: RetentionLockType
pattern RetentionLockType_NONE = RetentionLockType' "NONE"

{-# COMPLETE
  RetentionLockType_COMPLIANCE,
  RetentionLockType_GOVERNANCE,
  RetentionLockType_NONE,
  RetentionLockType'
  #-}
