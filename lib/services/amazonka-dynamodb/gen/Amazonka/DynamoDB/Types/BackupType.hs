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
-- Module      : Amazonka.DynamoDB.Types.BackupType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.BackupType
  ( BackupType
      ( ..,
        BackupType_AWS_BACKUP,
        BackupType_SYSTEM,
        BackupType_USER
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

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

pattern BackupType_AWS_BACKUP :: BackupType
pattern BackupType_AWS_BACKUP = BackupType' "AWS_BACKUP"

pattern BackupType_SYSTEM :: BackupType
pattern BackupType_SYSTEM = BackupType' "SYSTEM"

pattern BackupType_USER :: BackupType
pattern BackupType_USER = BackupType' "USER"

{-# COMPLETE
  BackupType_AWS_BACKUP,
  BackupType_SYSTEM,
  BackupType_USER,
  BackupType'
  #-}
