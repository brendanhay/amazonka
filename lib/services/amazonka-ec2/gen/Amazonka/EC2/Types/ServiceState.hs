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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ServiceState = ServiceState'
  { fromServiceState ::
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
