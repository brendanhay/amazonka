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
-- Module      : Network.AWS.FinSpace.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FinSpace.Types.EnvironmentStatus
  ( EnvironmentStatus
      ( ..,
        EnvironmentStatus_CREATED,
        EnvironmentStatus_CREATE_REQUESTED,
        EnvironmentStatus_CREATING,
        EnvironmentStatus_DELETED,
        EnvironmentStatus_DELETE_REQUESTED,
        EnvironmentStatus_DELETING,
        EnvironmentStatus_FAILED_CREATION,
        EnvironmentStatus_FAILED_DELETION,
        EnvironmentStatus_RETRY_DELETION,
        EnvironmentStatus_SUSPENDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EnvironmentStatus = EnvironmentStatus'
  { fromEnvironmentStatus ::
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

pattern EnvironmentStatus_CREATED :: EnvironmentStatus
pattern EnvironmentStatus_CREATED = EnvironmentStatus' "CREATED"

pattern EnvironmentStatus_CREATE_REQUESTED :: EnvironmentStatus
pattern EnvironmentStatus_CREATE_REQUESTED = EnvironmentStatus' "CREATE_REQUESTED"

pattern EnvironmentStatus_CREATING :: EnvironmentStatus
pattern EnvironmentStatus_CREATING = EnvironmentStatus' "CREATING"

pattern EnvironmentStatus_DELETED :: EnvironmentStatus
pattern EnvironmentStatus_DELETED = EnvironmentStatus' "DELETED"

pattern EnvironmentStatus_DELETE_REQUESTED :: EnvironmentStatus
pattern EnvironmentStatus_DELETE_REQUESTED = EnvironmentStatus' "DELETE_REQUESTED"

pattern EnvironmentStatus_DELETING :: EnvironmentStatus
pattern EnvironmentStatus_DELETING = EnvironmentStatus' "DELETING"

pattern EnvironmentStatus_FAILED_CREATION :: EnvironmentStatus
pattern EnvironmentStatus_FAILED_CREATION = EnvironmentStatus' "FAILED_CREATION"

pattern EnvironmentStatus_FAILED_DELETION :: EnvironmentStatus
pattern EnvironmentStatus_FAILED_DELETION = EnvironmentStatus' "FAILED_DELETION"

pattern EnvironmentStatus_RETRY_DELETION :: EnvironmentStatus
pattern EnvironmentStatus_RETRY_DELETION = EnvironmentStatus' "RETRY_DELETION"

pattern EnvironmentStatus_SUSPENDED :: EnvironmentStatus
pattern EnvironmentStatus_SUSPENDED = EnvironmentStatus' "SUSPENDED"

{-# COMPLETE
  EnvironmentStatus_CREATED,
  EnvironmentStatus_CREATE_REQUESTED,
  EnvironmentStatus_CREATING,
  EnvironmentStatus_DELETED,
  EnvironmentStatus_DELETE_REQUESTED,
  EnvironmentStatus_DELETING,
  EnvironmentStatus_FAILED_CREATION,
  EnvironmentStatus_FAILED_DELETION,
  EnvironmentStatus_RETRY_DELETION,
  EnvironmentStatus_SUSPENDED,
  EnvironmentStatus'
  #-}
