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

import qualified Network.AWS.Prelude as Prelude

newtype LifeCycleState = LifeCycleState'
  { fromLifeCycleState ::
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
