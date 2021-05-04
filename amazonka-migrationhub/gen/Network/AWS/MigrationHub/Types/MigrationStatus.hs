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
-- Module      : Network.AWS.MigrationHub.Types.MigrationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationStatus
  ( MigrationStatus
      ( ..,
        MigrationStatus_COMPLETED,
        MigrationStatus_FAILED,
        MigrationStatus_IN_PROGRESS,
        MigrationStatus_NOT_STARTED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype MigrationStatus = MigrationStatus'
  { fromMigrationStatus ::
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

pattern MigrationStatus_COMPLETED :: MigrationStatus
pattern MigrationStatus_COMPLETED = MigrationStatus' "COMPLETED"

pattern MigrationStatus_FAILED :: MigrationStatus
pattern MigrationStatus_FAILED = MigrationStatus' "FAILED"

pattern MigrationStatus_IN_PROGRESS :: MigrationStatus
pattern MigrationStatus_IN_PROGRESS = MigrationStatus' "IN_PROGRESS"

pattern MigrationStatus_NOT_STARTED :: MigrationStatus
pattern MigrationStatus_NOT_STARTED = MigrationStatus' "NOT_STARTED"

{-# COMPLETE
  MigrationStatus_COMPLETED,
  MigrationStatus_FAILED,
  MigrationStatus_IN_PROGRESS,
  MigrationStatus_NOT_STARTED,
  MigrationStatus'
  #-}
