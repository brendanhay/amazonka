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
-- Module      : Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EnvironmentLifecycleStatus = EnvironmentLifecycleStatus'
  { fromEnvironmentLifecycleStatus ::
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
