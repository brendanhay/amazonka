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
-- Module      : Network.AWS.DynamoDB.Types.BackupType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupType
  ( BackupType
      ( ..,
        BackupType_AWS_BACKUP,
        BackupType_SYSTEM,
        BackupType_USER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype BackupType = BackupType'
  { fromBackupType ::
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
