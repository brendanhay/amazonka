{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype BackupStatus = BackupStatus'
  { fromBackupStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
