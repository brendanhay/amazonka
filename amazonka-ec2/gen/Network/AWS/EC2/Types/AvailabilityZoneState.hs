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
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZoneState
  ( AvailabilityZoneState
      ( ..,
        AvailabilityZoneState_Available,
        AvailabilityZoneState_Impaired,
        AvailabilityZoneState_Information,
        AvailabilityZoneState_Unavailable
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype AvailabilityZoneState = AvailabilityZoneState'
  { fromAvailabilityZoneState ::
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
