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
-- Module      : Amazonka.FSx.Types.BackupType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.BackupType
  ( BackupType
      ( ..,
        BackupType_AUTOMATIC,
        BackupType_AWS_BACKUP,
        BackupType_USER_INITIATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The type of the backup.
newtype BackupType = BackupType'
  { fromBackupType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern BackupType_AUTOMATIC :: BackupType
pattern BackupType_AUTOMATIC = BackupType' "AUTOMATIC"

pattern BackupType_AWS_BACKUP :: BackupType
pattern BackupType_AWS_BACKUP = BackupType' "AWS_BACKUP"

pattern BackupType_USER_INITIATED :: BackupType
pattern BackupType_USER_INITIATED = BackupType' "USER_INITIATED"

{-# COMPLETE
  BackupType_AUTOMATIC,
  BackupType_AWS_BACKUP,
  BackupType_USER_INITIATED,
  BackupType'
  #-}
