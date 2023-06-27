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
-- Module      : Amazonka.EC2.Types.IpamResourceDiscoveryState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamResourceDiscoveryState
  ( IpamResourceDiscoveryState
      ( ..,
        IpamResourceDiscoveryState_Create_complete,
        IpamResourceDiscoveryState_Create_failed,
        IpamResourceDiscoveryState_Create_in_progress,
        IpamResourceDiscoveryState_Delete_complete,
        IpamResourceDiscoveryState_Delete_failed,
        IpamResourceDiscoveryState_Delete_in_progress,
        IpamResourceDiscoveryState_Isolate_complete,
        IpamResourceDiscoveryState_Isolate_in_progress,
        IpamResourceDiscoveryState_Modify_complete,
        IpamResourceDiscoveryState_Modify_failed,
        IpamResourceDiscoveryState_Modify_in_progress,
        IpamResourceDiscoveryState_Restore_in_progress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamResourceDiscoveryState = IpamResourceDiscoveryState'
  { fromIpamResourceDiscoveryState ::
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

pattern IpamResourceDiscoveryState_Create_complete :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Create_complete = IpamResourceDiscoveryState' "create-complete"

pattern IpamResourceDiscoveryState_Create_failed :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Create_failed = IpamResourceDiscoveryState' "create-failed"

pattern IpamResourceDiscoveryState_Create_in_progress :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Create_in_progress = IpamResourceDiscoveryState' "create-in-progress"

pattern IpamResourceDiscoveryState_Delete_complete :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Delete_complete = IpamResourceDiscoveryState' "delete-complete"

pattern IpamResourceDiscoveryState_Delete_failed :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Delete_failed = IpamResourceDiscoveryState' "delete-failed"

pattern IpamResourceDiscoveryState_Delete_in_progress :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Delete_in_progress = IpamResourceDiscoveryState' "delete-in-progress"

pattern IpamResourceDiscoveryState_Isolate_complete :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Isolate_complete = IpamResourceDiscoveryState' "isolate-complete"

pattern IpamResourceDiscoveryState_Isolate_in_progress :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Isolate_in_progress = IpamResourceDiscoveryState' "isolate-in-progress"

pattern IpamResourceDiscoveryState_Modify_complete :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Modify_complete = IpamResourceDiscoveryState' "modify-complete"

pattern IpamResourceDiscoveryState_Modify_failed :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Modify_failed = IpamResourceDiscoveryState' "modify-failed"

pattern IpamResourceDiscoveryState_Modify_in_progress :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Modify_in_progress = IpamResourceDiscoveryState' "modify-in-progress"

pattern IpamResourceDiscoveryState_Restore_in_progress :: IpamResourceDiscoveryState
pattern IpamResourceDiscoveryState_Restore_in_progress = IpamResourceDiscoveryState' "restore-in-progress"

{-# COMPLETE
  IpamResourceDiscoveryState_Create_complete,
  IpamResourceDiscoveryState_Create_failed,
  IpamResourceDiscoveryState_Create_in_progress,
  IpamResourceDiscoveryState_Delete_complete,
  IpamResourceDiscoveryState_Delete_failed,
  IpamResourceDiscoveryState_Delete_in_progress,
  IpamResourceDiscoveryState_Isolate_complete,
  IpamResourceDiscoveryState_Isolate_in_progress,
  IpamResourceDiscoveryState_Modify_complete,
  IpamResourceDiscoveryState_Modify_failed,
  IpamResourceDiscoveryState_Modify_in_progress,
  IpamResourceDiscoveryState_Restore_in_progress,
  IpamResourceDiscoveryState'
  #-}
