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
-- Module      : Amazonka.DynamoDB.Types.BackupTypeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.BackupTypeFilter
  ( BackupTypeFilter
      ( ..,
        BackupTypeFilter_ALL,
        BackupTypeFilter_AWS_BACKUP,
        BackupTypeFilter_SYSTEM,
        BackupTypeFilter_USER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype BackupTypeFilter = BackupTypeFilter'
  { fromBackupTypeFilter ::
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
