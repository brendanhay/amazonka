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
-- Module      : Amazonka.EC2.Types.AvailabilityZoneState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AvailabilityZoneState
  ( AvailabilityZoneState
      ( ..,
        AvailabilityZoneState_Available,
        AvailabilityZoneState_Impaired,
        AvailabilityZoneState_Information,
        AvailabilityZoneState_Unavailable
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype AvailabilityZoneState = AvailabilityZoneState'
  { fromAvailabilityZoneState ::
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

pattern AvailabilityZoneState_Available :: AvailabilityZoneState
pattern AvailabilityZoneState_Available = AvailabilityZoneState' "available"

pattern AvailabilityZoneState_Impaired :: AvailabilityZoneState
pattern AvailabilityZoneState_Impaired = AvailabilityZoneState' "impaired"

pattern AvailabilityZoneState_Information :: AvailabilityZoneState
pattern AvailabilityZoneState_Information = AvailabilityZoneState' "information"

pattern AvailabilityZoneState_Unavailable :: AvailabilityZoneState
pattern AvailabilityZoneState_Unavailable = AvailabilityZoneState' "unavailable"

{-# COMPLETE
  AvailabilityZoneState_Available,
  AvailabilityZoneState_Impaired,
  AvailabilityZoneState_Information,
  AvailabilityZoneState_Unavailable,
  AvailabilityZoneState'
  #-}
