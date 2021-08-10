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
-- Module      : Network.AWS.DynamoDB.Types.BackupTypeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupTypeFilter
  ( BackupTypeFilter
      ( ..,
        BackupTypeFilter_ALL,
        BackupTypeFilter_AWS_BACKUP,
        BackupTypeFilter_SYSTEM,
        BackupTypeFilter_USER
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype BackupTypeFilter = BackupTypeFilter'
  { fromBackupTypeFilter ::
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

pattern BackupTypeFilter_ALL :: BackupTypeFilter
pattern BackupTypeFilter_ALL = BackupTypeFilter' "ALL"

pattern BackupTypeFilter_AWS_BACKUP :: BackupTypeFilter
pattern BackupTypeFilter_AWS_BACKUP = BackupTypeFilter' "AWS_BACKUP"

pattern BackupTypeFilter_SYSTEM :: BackupTypeFilter
pattern BackupTypeFilter_SYSTEM = BackupTypeFilter' "SYSTEM"

pattern BackupTypeFilter_USER :: BackupTypeFilter
pattern BackupTypeFilter_USER = BackupTypeFilter' "USER"

{-# COMPLETE
  BackupTypeFilter_ALL,
  BackupTypeFilter_AWS_BACKUP,
  BackupTypeFilter_SYSTEM,
  BackupTypeFilter_USER,
  BackupTypeFilter'
  #-}
