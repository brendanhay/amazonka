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
-- Module      : Amazonka.EC2.Types.IpamPoolCidrState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamPoolCidrState
  ( IpamPoolCidrState
      ( ..,
        IpamPoolCidrState_Deprovisioned,
        IpamPoolCidrState_Failed_deprovision,
        IpamPoolCidrState_Failed_import,
        IpamPoolCidrState_Failed_provision,
        IpamPoolCidrState_Pending_deprovision,
        IpamPoolCidrState_Pending_import,
        IpamPoolCidrState_Pending_provision,
        IpamPoolCidrState_Provisioned
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IpamPoolCidrState = IpamPoolCidrState'
  { fromIpamPoolCidrState ::
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

pattern IpamPoolCidrState_Deprovisioned :: IpamPoolCidrState
pattern IpamPoolCidrState_Deprovisioned = IpamPoolCidrState' "deprovisioned"

pattern IpamPoolCidrState_Failed_deprovision :: IpamPoolCidrState
pattern IpamPoolCidrState_Failed_deprovision = IpamPoolCidrState' "failed-deprovision"

pattern IpamPoolCidrState_Failed_import :: IpamPoolCidrState
pattern IpamPoolCidrState_Failed_import = IpamPoolCidrState' "failed-import"

pattern IpamPoolCidrState_Failed_provision :: IpamPoolCidrState
pattern IpamPoolCidrState_Failed_provision = IpamPoolCidrState' "failed-provision"

pattern IpamPoolCidrState_Pending_deprovision :: IpamPoolCidrState
pattern IpamPoolCidrState_Pending_deprovision = IpamPoolCidrState' "pending-deprovision"

pattern IpamPoolCidrState_Pending_import :: IpamPoolCidrState
pattern IpamPoolCidrState_Pending_import = IpamPoolCidrState' "pending-import"

pattern IpamPoolCidrState_Pending_provision :: IpamPoolCidrState
pattern IpamPoolCidrState_Pending_provision = IpamPoolCidrState' "pending-provision"

pattern IpamPoolCidrState_Provisioned :: IpamPoolCidrState
pattern IpamPoolCidrState_Provisioned = IpamPoolCidrState' "provisioned"

{-# COMPLETE
  IpamPoolCidrState_Deprovisioned,
  IpamPoolCidrState_Failed_deprovision,
  IpamPoolCidrState_Failed_import,
  IpamPoolCidrState_Failed_provision,
  IpamPoolCidrState_Pending_deprovision,
  IpamPoolCidrState_Pending_import,
  IpamPoolCidrState_Pending_provision,
  IpamPoolCidrState_Provisioned,
  IpamPoolCidrState'
  #-}
