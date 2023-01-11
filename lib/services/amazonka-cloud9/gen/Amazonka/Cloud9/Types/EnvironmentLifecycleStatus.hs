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
-- Module      : Amazonka.Cloud9.Types.EnvironmentLifecycleStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Types.EnvironmentLifecycleStatus
  ( EnvironmentLifecycleStatus
      ( ..,
        EnvironmentLifecycleStatus_CREATED,
        EnvironmentLifecycleStatus_CREATE_FAILED,
        EnvironmentLifecycleStatus_CREATING,
        EnvironmentLifecycleStatus_DELETE_FAILED,
        EnvironmentLifecycleStatus_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentLifecycleStatus = EnvironmentLifecycleStatus'
  { fromEnvironmentLifecycleStatus ::
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

pattern EnvironmentLifecycleStatus_CREATED :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatus_CREATED = EnvironmentLifecycleStatus' "CREATED"

pattern EnvironmentLifecycleStatus_CREATE_FAILED :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatus_CREATE_FAILED = EnvironmentLifecycleStatus' "CREATE_FAILED"

pattern EnvironmentLifecycleStatus_CREATING :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatus_CREATING = EnvironmentLifecycleStatus' "CREATING"

pattern EnvironmentLifecycleStatus_DELETE_FAILED :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatus_DELETE_FAILED = EnvironmentLifecycleStatus' "DELETE_FAILED"

pattern EnvironmentLifecycleStatus_DELETING :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatus_DELETING = EnvironmentLifecycleStatus' "DELETING"

{-# COMPLETE
  EnvironmentLifecycleStatus_CREATED,
  EnvironmentLifecycleStatus_CREATE_FAILED,
  EnvironmentLifecycleStatus_CREATING,
  EnvironmentLifecycleStatus_DELETE_FAILED,
  EnvironmentLifecycleStatus_DELETING,
  EnvironmentLifecycleStatus'
  #-}
