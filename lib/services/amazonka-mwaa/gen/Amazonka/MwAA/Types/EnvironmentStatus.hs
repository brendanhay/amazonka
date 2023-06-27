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
-- Module      : Amazonka.MwAA.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.EnvironmentStatus
  ( EnvironmentStatus
      ( ..,
        EnvironmentStatus_AVAILABLE,
        EnvironmentStatus_CREATE_FAILED,
        EnvironmentStatus_CREATING,
        EnvironmentStatus_CREATING_SNAPSHOT,
        EnvironmentStatus_DELETED,
        EnvironmentStatus_DELETING,
        EnvironmentStatus_ROLLING_BACK,
        EnvironmentStatus_UNAVAILABLE,
        EnvironmentStatus_UPDATE_FAILED,
        EnvironmentStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentStatus = EnvironmentStatus'
  { fromEnvironmentStatus ::
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

pattern EnvironmentStatus_AVAILABLE :: EnvironmentStatus
pattern EnvironmentStatus_AVAILABLE = EnvironmentStatus' "AVAILABLE"

pattern EnvironmentStatus_CREATE_FAILED :: EnvironmentStatus
pattern EnvironmentStatus_CREATE_FAILED = EnvironmentStatus' "CREATE_FAILED"

pattern EnvironmentStatus_CREATING :: EnvironmentStatus
pattern EnvironmentStatus_CREATING = EnvironmentStatus' "CREATING"

pattern EnvironmentStatus_CREATING_SNAPSHOT :: EnvironmentStatus
pattern EnvironmentStatus_CREATING_SNAPSHOT = EnvironmentStatus' "CREATING_SNAPSHOT"

pattern EnvironmentStatus_DELETED :: EnvironmentStatus
pattern EnvironmentStatus_DELETED = EnvironmentStatus' "DELETED"

pattern EnvironmentStatus_DELETING :: EnvironmentStatus
pattern EnvironmentStatus_DELETING = EnvironmentStatus' "DELETING"

pattern EnvironmentStatus_ROLLING_BACK :: EnvironmentStatus
pattern EnvironmentStatus_ROLLING_BACK = EnvironmentStatus' "ROLLING_BACK"

pattern EnvironmentStatus_UNAVAILABLE :: EnvironmentStatus
pattern EnvironmentStatus_UNAVAILABLE = EnvironmentStatus' "UNAVAILABLE"

pattern EnvironmentStatus_UPDATE_FAILED :: EnvironmentStatus
pattern EnvironmentStatus_UPDATE_FAILED = EnvironmentStatus' "UPDATE_FAILED"

pattern EnvironmentStatus_UPDATING :: EnvironmentStatus
pattern EnvironmentStatus_UPDATING = EnvironmentStatus' "UPDATING"

{-# COMPLETE
  EnvironmentStatus_AVAILABLE,
  EnvironmentStatus_CREATE_FAILED,
  EnvironmentStatus_CREATING,
  EnvironmentStatus_CREATING_SNAPSHOT,
  EnvironmentStatus_DELETED,
  EnvironmentStatus_DELETING,
  EnvironmentStatus_ROLLING_BACK,
  EnvironmentStatus_UNAVAILABLE,
  EnvironmentStatus_UPDATE_FAILED,
  EnvironmentStatus_UPDATING,
  EnvironmentStatus'
  #-}
