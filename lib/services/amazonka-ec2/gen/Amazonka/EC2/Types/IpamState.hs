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
-- Module      : Amazonka.EC2.Types.IpamState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamState
  ( IpamState
      ( ..,
        IpamState_Create_complete,
        IpamState_Create_failed,
        IpamState_Create_in_progress,
        IpamState_Delete_complete,
        IpamState_Delete_failed,
        IpamState_Delete_in_progress,
        IpamState_Isolate_complete,
        IpamState_Isolate_in_progress,
        IpamState_Modify_complete,
        IpamState_Modify_failed,
        IpamState_Modify_in_progress,
        IpamState_Restore_in_progress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamState = IpamState'
  { fromIpamState ::
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

pattern IpamState_Create_complete :: IpamState
pattern IpamState_Create_complete = IpamState' "create-complete"

pattern IpamState_Create_failed :: IpamState
pattern IpamState_Create_failed = IpamState' "create-failed"

pattern IpamState_Create_in_progress :: IpamState
pattern IpamState_Create_in_progress = IpamState' "create-in-progress"

pattern IpamState_Delete_complete :: IpamState
pattern IpamState_Delete_complete = IpamState' "delete-complete"

pattern IpamState_Delete_failed :: IpamState
pattern IpamState_Delete_failed = IpamState' "delete-failed"

pattern IpamState_Delete_in_progress :: IpamState
pattern IpamState_Delete_in_progress = IpamState' "delete-in-progress"

pattern IpamState_Isolate_complete :: IpamState
pattern IpamState_Isolate_complete = IpamState' "isolate-complete"

pattern IpamState_Isolate_in_progress :: IpamState
pattern IpamState_Isolate_in_progress = IpamState' "isolate-in-progress"

pattern IpamState_Modify_complete :: IpamState
pattern IpamState_Modify_complete = IpamState' "modify-complete"

pattern IpamState_Modify_failed :: IpamState
pattern IpamState_Modify_failed = IpamState' "modify-failed"

pattern IpamState_Modify_in_progress :: IpamState
pattern IpamState_Modify_in_progress = IpamState' "modify-in-progress"

pattern IpamState_Restore_in_progress :: IpamState
pattern IpamState_Restore_in_progress = IpamState' "restore-in-progress"

{-# COMPLETE
  IpamState_Create_complete,
  IpamState_Create_failed,
  IpamState_Create_in_progress,
  IpamState_Delete_complete,
  IpamState_Delete_failed,
  IpamState_Delete_in_progress,
  IpamState_Isolate_complete,
  IpamState_Isolate_in_progress,
  IpamState_Modify_complete,
  IpamState_Modify_failed,
  IpamState_Modify_in_progress,
  IpamState_Restore_in_progress,
  IpamState'
  #-}
