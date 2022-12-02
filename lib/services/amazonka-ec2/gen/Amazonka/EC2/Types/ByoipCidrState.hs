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
-- Module      : Amazonka.EC2.Types.ByoipCidrState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ByoipCidrState
  ( ByoipCidrState
      ( ..,
        ByoipCidrState_Advertised,
        ByoipCidrState_Deprovisioned,
        ByoipCidrState_Failed_deprovision,
        ByoipCidrState_Failed_provision,
        ByoipCidrState_Pending_deprovision,
        ByoipCidrState_Pending_provision,
        ByoipCidrState_Provisioned,
        ByoipCidrState_Provisioned_not_publicly_advertisable
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ByoipCidrState = ByoipCidrState'
  { fromByoipCidrState ::
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

pattern ByoipCidrState_Advertised :: ByoipCidrState
pattern ByoipCidrState_Advertised = ByoipCidrState' "advertised"

pattern ByoipCidrState_Deprovisioned :: ByoipCidrState
pattern ByoipCidrState_Deprovisioned = ByoipCidrState' "deprovisioned"

pattern ByoipCidrState_Failed_deprovision :: ByoipCidrState
pattern ByoipCidrState_Failed_deprovision = ByoipCidrState' "failed-deprovision"

pattern ByoipCidrState_Failed_provision :: ByoipCidrState
pattern ByoipCidrState_Failed_provision = ByoipCidrState' "failed-provision"

pattern ByoipCidrState_Pending_deprovision :: ByoipCidrState
pattern ByoipCidrState_Pending_deprovision = ByoipCidrState' "pending-deprovision"

pattern ByoipCidrState_Pending_provision :: ByoipCidrState
pattern ByoipCidrState_Pending_provision = ByoipCidrState' "pending-provision"

pattern ByoipCidrState_Provisioned :: ByoipCidrState
pattern ByoipCidrState_Provisioned = ByoipCidrState' "provisioned"

pattern ByoipCidrState_Provisioned_not_publicly_advertisable :: ByoipCidrState
pattern ByoipCidrState_Provisioned_not_publicly_advertisable = ByoipCidrState' "provisioned-not-publicly-advertisable"

{-# COMPLETE
  ByoipCidrState_Advertised,
  ByoipCidrState_Deprovisioned,
  ByoipCidrState_Failed_deprovision,
  ByoipCidrState_Failed_provision,
  ByoipCidrState_Pending_deprovision,
  ByoipCidrState_Pending_provision,
  ByoipCidrState_Provisioned,
  ByoipCidrState_Provisioned_not_publicly_advertisable,
  ByoipCidrState'
  #-}
