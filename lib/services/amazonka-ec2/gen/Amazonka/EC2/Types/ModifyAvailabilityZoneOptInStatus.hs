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
-- Module      : Amazonka.EC2.Types.ModifyAvailabilityZoneOptInStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ModifyAvailabilityZoneOptInStatus
  ( ModifyAvailabilityZoneOptInStatus
      ( ..,
        ModifyAvailabilityZoneOptInStatus_Not_opted_in,
        ModifyAvailabilityZoneOptInStatus_Opted_in
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ModifyAvailabilityZoneOptInStatus = ModifyAvailabilityZoneOptInStatus'
  { fromModifyAvailabilityZoneOptInStatus ::
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

pattern ModifyAvailabilityZoneOptInStatus_Not_opted_in :: ModifyAvailabilityZoneOptInStatus
pattern ModifyAvailabilityZoneOptInStatus_Not_opted_in = ModifyAvailabilityZoneOptInStatus' "not-opted-in"

pattern ModifyAvailabilityZoneOptInStatus_Opted_in :: ModifyAvailabilityZoneOptInStatus
pattern ModifyAvailabilityZoneOptInStatus_Opted_in = ModifyAvailabilityZoneOptInStatus' "opted-in"

{-# COMPLETE
  ModifyAvailabilityZoneOptInStatus_Not_opted_in,
  ModifyAvailabilityZoneOptInStatus_Opted_in,
  ModifyAvailabilityZoneOptInStatus'
  #-}
