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
-- Module      : Amazonka.EC2.Types.IpamScopeState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamScopeState
  ( IpamScopeState
      ( ..,
        IpamScopeState_Create_complete,
        IpamScopeState_Create_failed,
        IpamScopeState_Create_in_progress,
        IpamScopeState_Delete_complete,
        IpamScopeState_Delete_failed,
        IpamScopeState_Delete_in_progress,
        IpamScopeState_Isolate_complete,
        IpamScopeState_Isolate_in_progress,
        IpamScopeState_Modify_complete,
        IpamScopeState_Modify_failed,
        IpamScopeState_Modify_in_progress,
        IpamScopeState_Restore_in_progress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamScopeState = IpamScopeState'
  { fromIpamScopeState ::
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

pattern IpamScopeState_Create_complete :: IpamScopeState
pattern IpamScopeState_Create_complete = IpamScopeState' "create-complete"

pattern IpamScopeState_Create_failed :: IpamScopeState
pattern IpamScopeState_Create_failed = IpamScopeState' "create-failed"

pattern IpamScopeState_Create_in_progress :: IpamScopeState
pattern IpamScopeState_Create_in_progress = IpamScopeState' "create-in-progress"

pattern IpamScopeState_Delete_complete :: IpamScopeState
pattern IpamScopeState_Delete_complete = IpamScopeState' "delete-complete"

pattern IpamScopeState_Delete_failed :: IpamScopeState
pattern IpamScopeState_Delete_failed = IpamScopeState' "delete-failed"

pattern IpamScopeState_Delete_in_progress :: IpamScopeState
pattern IpamScopeState_Delete_in_progress = IpamScopeState' "delete-in-progress"

pattern IpamScopeState_Isolate_complete :: IpamScopeState
pattern IpamScopeState_Isolate_complete = IpamScopeState' "isolate-complete"

pattern IpamScopeState_Isolate_in_progress :: IpamScopeState
pattern IpamScopeState_Isolate_in_progress = IpamScopeState' "isolate-in-progress"

pattern IpamScopeState_Modify_complete :: IpamScopeState
pattern IpamScopeState_Modify_complete = IpamScopeState' "modify-complete"

pattern IpamScopeState_Modify_failed :: IpamScopeState
pattern IpamScopeState_Modify_failed = IpamScopeState' "modify-failed"

pattern IpamScopeState_Modify_in_progress :: IpamScopeState
pattern IpamScopeState_Modify_in_progress = IpamScopeState' "modify-in-progress"

pattern IpamScopeState_Restore_in_progress :: IpamScopeState
pattern IpamScopeState_Restore_in_progress = IpamScopeState' "restore-in-progress"

{-# COMPLETE
  IpamScopeState_Create_complete,
  IpamScopeState_Create_failed,
  IpamScopeState_Create_in_progress,
  IpamScopeState_Delete_complete,
  IpamScopeState_Delete_failed,
  IpamScopeState_Delete_in_progress,
  IpamScopeState_Isolate_complete,
  IpamScopeState_Isolate_in_progress,
  IpamScopeState_Modify_complete,
  IpamScopeState_Modify_failed,
  IpamScopeState_Modify_in_progress,
  IpamScopeState_Restore_in_progress,
  IpamScopeState'
  #-}
