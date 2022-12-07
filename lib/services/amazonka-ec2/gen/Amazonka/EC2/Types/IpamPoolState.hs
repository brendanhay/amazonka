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
-- Module      : Amazonka.EC2.Types.IpamPoolState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamPoolState
  ( IpamPoolState
      ( ..,
        IpamPoolState_Create_complete,
        IpamPoolState_Create_failed,
        IpamPoolState_Create_in_progress,
        IpamPoolState_Delete_complete,
        IpamPoolState_Delete_failed,
        IpamPoolState_Delete_in_progress,
        IpamPoolState_Isolate_complete,
        IpamPoolState_Isolate_in_progress,
        IpamPoolState_Modify_complete,
        IpamPoolState_Modify_failed,
        IpamPoolState_Modify_in_progress,
        IpamPoolState_Restore_in_progress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamPoolState = IpamPoolState'
  { fromIpamPoolState ::
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

pattern IpamPoolState_Create_complete :: IpamPoolState
pattern IpamPoolState_Create_complete = IpamPoolState' "create-complete"

pattern IpamPoolState_Create_failed :: IpamPoolState
pattern IpamPoolState_Create_failed = IpamPoolState' "create-failed"

pattern IpamPoolState_Create_in_progress :: IpamPoolState
pattern IpamPoolState_Create_in_progress = IpamPoolState' "create-in-progress"

pattern IpamPoolState_Delete_complete :: IpamPoolState
pattern IpamPoolState_Delete_complete = IpamPoolState' "delete-complete"

pattern IpamPoolState_Delete_failed :: IpamPoolState
pattern IpamPoolState_Delete_failed = IpamPoolState' "delete-failed"

pattern IpamPoolState_Delete_in_progress :: IpamPoolState
pattern IpamPoolState_Delete_in_progress = IpamPoolState' "delete-in-progress"

pattern IpamPoolState_Isolate_complete :: IpamPoolState
pattern IpamPoolState_Isolate_complete = IpamPoolState' "isolate-complete"

pattern IpamPoolState_Isolate_in_progress :: IpamPoolState
pattern IpamPoolState_Isolate_in_progress = IpamPoolState' "isolate-in-progress"

pattern IpamPoolState_Modify_complete :: IpamPoolState
pattern IpamPoolState_Modify_complete = IpamPoolState' "modify-complete"

pattern IpamPoolState_Modify_failed :: IpamPoolState
pattern IpamPoolState_Modify_failed = IpamPoolState' "modify-failed"

pattern IpamPoolState_Modify_in_progress :: IpamPoolState
pattern IpamPoolState_Modify_in_progress = IpamPoolState' "modify-in-progress"

pattern IpamPoolState_Restore_in_progress :: IpamPoolState
pattern IpamPoolState_Restore_in_progress = IpamPoolState' "restore-in-progress"

{-# COMPLETE
  IpamPoolState_Create_complete,
  IpamPoolState_Create_failed,
  IpamPoolState_Create_in_progress,
  IpamPoolState_Delete_complete,
  IpamPoolState_Delete_failed,
  IpamPoolState_Delete_in_progress,
  IpamPoolState_Isolate_complete,
  IpamPoolState_Isolate_in_progress,
  IpamPoolState_Modify_complete,
  IpamPoolState_Modify_failed,
  IpamPoolState_Modify_in_progress,
  IpamPoolState_Restore_in_progress,
  IpamPoolState'
  #-}
