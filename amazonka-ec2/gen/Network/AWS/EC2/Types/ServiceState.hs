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
-- Module      : Network.AWS.EC2.Types.ServiceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceState
  ( ServiceState
      ( ..,
        ServiceState_Available,
        ServiceState_Deleted,
        ServiceState_Deleting,
        ServiceState_Failed,
        ServiceState_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ServiceState = ServiceState'
  { fromServiceState ::
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

pattern ServiceState_Available :: ServiceState
pattern ServiceState_Available = ServiceState' "Available"

pattern ServiceState_Deleted :: ServiceState
pattern ServiceState_Deleted = ServiceState' "Deleted"

pattern ServiceState_Deleting :: ServiceState
pattern ServiceState_Deleting = ServiceState' "Deleting"

pattern ServiceState_Failed :: ServiceState
pattern ServiceState_Failed = ServiceState' "Failed"

pattern ServiceState_Pending :: ServiceState
pattern ServiceState_Pending = ServiceState' "Pending"

{-# COMPLETE
  ServiceState_Available,
  ServiceState_Deleted,
  ServiceState_Deleting,
  ServiceState_Failed,
  ServiceState_Pending,
  ServiceState'
  #-}
