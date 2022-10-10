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
-- Module      : Amazonka.EC2.Types.ServiceState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ServiceState
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

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ServiceState = ServiceState'
  { fromServiceState ::
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
