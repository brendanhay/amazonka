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
-- Module      : Amazonka.EC2.Types.AvailabilityZoneOptInStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AvailabilityZoneOptInStatus
  ( AvailabilityZoneOptInStatus
      ( ..,
        AvailabilityZoneOptInStatus_Not_opted_in,
        AvailabilityZoneOptInStatus_Opt_in_not_required,
        AvailabilityZoneOptInStatus_Opted_in
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype AvailabilityZoneOptInStatus = AvailabilityZoneOptInStatus'
  { fromAvailabilityZoneOptInStatus ::
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

pattern AvailabilityZoneOptInStatus_Not_opted_in :: AvailabilityZoneOptInStatus
pattern AvailabilityZoneOptInStatus_Not_opted_in = AvailabilityZoneOptInStatus' "not-opted-in"

pattern AvailabilityZoneOptInStatus_Opt_in_not_required :: AvailabilityZoneOptInStatus
pattern AvailabilityZoneOptInStatus_Opt_in_not_required = AvailabilityZoneOptInStatus' "opt-in-not-required"

pattern AvailabilityZoneOptInStatus_Opted_in :: AvailabilityZoneOptInStatus
pattern AvailabilityZoneOptInStatus_Opted_in = AvailabilityZoneOptInStatus' "opted-in"

{-# COMPLETE
  AvailabilityZoneOptInStatus_Not_opted_in,
  AvailabilityZoneOptInStatus_Opt_in_not_required,
  AvailabilityZoneOptInStatus_Opted_in,
  AvailabilityZoneOptInStatus'
  #-}
