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
-- Module      : Network.AWS.EFS.Types.LifeCycleState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifeCycleState
  ( LifeCycleState
      ( ..,
        LifeCycleState_Available,
        LifeCycleState_Creating,
        LifeCycleState_Deleted,
        LifeCycleState_Deleting,
        LifeCycleState_Updating
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LifeCycleState = LifeCycleState'
  { fromLifeCycleState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern LifeCycleState_Available :: LifeCycleState
pattern LifeCycleState_Available = LifeCycleState' "available"

pattern LifeCycleState_Creating :: LifeCycleState
pattern LifeCycleState_Creating = LifeCycleState' "creating"

pattern LifeCycleState_Deleted :: LifeCycleState
pattern LifeCycleState_Deleted = LifeCycleState' "deleted"

pattern LifeCycleState_Deleting :: LifeCycleState
pattern LifeCycleState_Deleting = LifeCycleState' "deleting"

pattern LifeCycleState_Updating :: LifeCycleState
pattern LifeCycleState_Updating = LifeCycleState' "updating"

{-# COMPLETE
  LifeCycleState_Available,
  LifeCycleState_Creating,
  LifeCycleState_Deleted,
  LifeCycleState_Deleting,
  LifeCycleState_Updating,
  LifeCycleState'
  #-}
