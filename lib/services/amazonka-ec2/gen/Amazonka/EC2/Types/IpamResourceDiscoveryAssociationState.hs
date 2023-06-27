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
-- Module      : Amazonka.EC2.Types.IpamResourceDiscoveryAssociationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamResourceDiscoveryAssociationState
  ( IpamResourceDiscoveryAssociationState
      ( ..,
        IpamResourceDiscoveryAssociationState_Associate_complete,
        IpamResourceDiscoveryAssociationState_Associate_failed,
        IpamResourceDiscoveryAssociationState_Associate_in_progress,
        IpamResourceDiscoveryAssociationState_Disassociate_complete,
        IpamResourceDiscoveryAssociationState_Disassociate_failed,
        IpamResourceDiscoveryAssociationState_Disassociate_in_progress,
        IpamResourceDiscoveryAssociationState_Isolate_complete,
        IpamResourceDiscoveryAssociationState_Isolate_in_progress,
        IpamResourceDiscoveryAssociationState_Restore_in_progress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamResourceDiscoveryAssociationState = IpamResourceDiscoveryAssociationState'
  { fromIpamResourceDiscoveryAssociationState ::
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

pattern IpamResourceDiscoveryAssociationState_Associate_complete :: IpamResourceDiscoveryAssociationState
pattern IpamResourceDiscoveryAssociationState_Associate_complete = IpamResourceDiscoveryAssociationState' "associate-complete"

pattern IpamResourceDiscoveryAssociationState_Associate_failed :: IpamResourceDiscoveryAssociationState
pattern IpamResourceDiscoveryAssociationState_Associate_failed = IpamResourceDiscoveryAssociationState' "associate-failed"

pattern IpamResourceDiscoveryAssociationState_Associate_in_progress :: IpamResourceDiscoveryAssociationState
pattern IpamResourceDiscoveryAssociationState_Associate_in_progress = IpamResourceDiscoveryAssociationState' "associate-in-progress"

pattern IpamResourceDiscoveryAssociationState_Disassociate_complete :: IpamResourceDiscoveryAssociationState
pattern IpamResourceDiscoveryAssociationState_Disassociate_complete = IpamResourceDiscoveryAssociationState' "disassociate-complete"

pattern IpamResourceDiscoveryAssociationState_Disassociate_failed :: IpamResourceDiscoveryAssociationState
pattern IpamResourceDiscoveryAssociationState_Disassociate_failed = IpamResourceDiscoveryAssociationState' "disassociate-failed"

pattern IpamResourceDiscoveryAssociationState_Disassociate_in_progress :: IpamResourceDiscoveryAssociationState
pattern IpamResourceDiscoveryAssociationState_Disassociate_in_progress = IpamResourceDiscoveryAssociationState' "disassociate-in-progress"

pattern IpamResourceDiscoveryAssociationState_Isolate_complete :: IpamResourceDiscoveryAssociationState
pattern IpamResourceDiscoveryAssociationState_Isolate_complete = IpamResourceDiscoveryAssociationState' "isolate-complete"

pattern IpamResourceDiscoveryAssociationState_Isolate_in_progress :: IpamResourceDiscoveryAssociationState
pattern IpamResourceDiscoveryAssociationState_Isolate_in_progress = IpamResourceDiscoveryAssociationState' "isolate-in-progress"

pattern IpamResourceDiscoveryAssociationState_Restore_in_progress :: IpamResourceDiscoveryAssociationState
pattern IpamResourceDiscoveryAssociationState_Restore_in_progress = IpamResourceDiscoveryAssociationState' "restore-in-progress"

{-# COMPLETE
  IpamResourceDiscoveryAssociationState_Associate_complete,
  IpamResourceDiscoveryAssociationState_Associate_failed,
  IpamResourceDiscoveryAssociationState_Associate_in_progress,
  IpamResourceDiscoveryAssociationState_Disassociate_complete,
  IpamResourceDiscoveryAssociationState_Disassociate_failed,
  IpamResourceDiscoveryAssociationState_Disassociate_in_progress,
  IpamResourceDiscoveryAssociationState_Isolate_complete,
  IpamResourceDiscoveryAssociationState_Isolate_in_progress,
  IpamResourceDiscoveryAssociationState_Restore_in_progress,
  IpamResourceDiscoveryAssociationState'
  #-}
