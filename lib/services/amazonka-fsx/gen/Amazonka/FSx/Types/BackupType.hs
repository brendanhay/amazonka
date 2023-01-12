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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of the backup.
newtype BackupType = BackupType'
  { fromBackupType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
