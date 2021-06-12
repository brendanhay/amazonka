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
-- Module      : Network.AWS.EFS.Types.BackupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.BackupStatus
  ( BackupStatus
      ( ..,
        BackupStatus_DISABLED,
        BackupStatus_DISABLING,
        BackupStatus_ENABLED,
        BackupStatus_ENABLING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype BackupStatus = BackupStatus'
  { fromBackupStatus ::
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

pattern BackupStatus_DISABLED :: BackupStatus
pattern BackupStatus_DISABLED = BackupStatus' "DISABLED"

pattern BackupStatus_DISABLING :: BackupStatus
pattern BackupStatus_DISABLING = BackupStatus' "DISABLING"

pattern BackupStatus_ENABLED :: BackupStatus
pattern BackupStatus_ENABLED = BackupStatus' "ENABLED"

pattern BackupStatus_ENABLING :: BackupStatus
pattern BackupStatus_ENABLING = BackupStatus' "ENABLING"

{-# COMPLETE
  BackupStatus_DISABLED,
  BackupStatus_DISABLING,
  BackupStatus_ENABLED,
  BackupStatus_ENABLING,
  BackupStatus'
  #-}
