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
-- Module      : Amazonka.EFS.Types.LifeCycleState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.LifeCycleState
  ( LifeCycleState
      ( ..,
        LifeCycleState_Available,
        LifeCycleState_Creating,
        LifeCycleState_Deleted,
        LifeCycleState_Deleting,
        LifeCycleState_Error,
        LifeCycleState_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LifeCycleState = LifeCycleState'
  { fromLifeCycleState ::
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

pattern LifeCycleState_Available :: LifeCycleState
pattern LifeCycleState_Available = LifeCycleState' "available"

pattern LifeCycleState_Creating :: LifeCycleState
pattern LifeCycleState_Creating = LifeCycleState' "creating"

pattern LifeCycleState_Deleted :: LifeCycleState
pattern LifeCycleState_Deleted = LifeCycleState' "deleted"

pattern LifeCycleState_Deleting :: LifeCycleState
pattern LifeCycleState_Deleting = LifeCycleState' "deleting"

pattern LifeCycleState_Error :: LifeCycleState
pattern LifeCycleState_Error = LifeCycleState' "error"

pattern LifeCycleState_Updating :: LifeCycleState
pattern LifeCycleState_Updating = LifeCycleState' "updating"

{-# COMPLETE
  LifeCycleState_Available,
  LifeCycleState_Creating,
  LifeCycleState_Deleted,
  LifeCycleState_Deleting,
  LifeCycleState_Error,
  LifeCycleState_Updating,
  LifeCycleState'
  #-}
