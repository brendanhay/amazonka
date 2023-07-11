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
-- Module      : Amazonka.MediaLive.Types.Scte35DeviceRestrictions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35DeviceRestrictions
  ( Scte35DeviceRestrictions
      ( ..,
        Scte35DeviceRestrictions_NONE,
        Scte35DeviceRestrictions_RESTRICT_GROUP0,
        Scte35DeviceRestrictions_RESTRICT_GROUP1,
        Scte35DeviceRestrictions_RESTRICT_GROUP2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Corresponds to the device_restrictions parameter in a
-- segmentation_descriptor. If you include one of the \"restriction\" flags
-- then you must include all four of them.
newtype Scte35DeviceRestrictions = Scte35DeviceRestrictions'
  { fromScte35DeviceRestrictions ::
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

pattern Scte35DeviceRestrictions_NONE :: Scte35DeviceRestrictions
pattern Scte35DeviceRestrictions_NONE = Scte35DeviceRestrictions' "NONE"

pattern Scte35DeviceRestrictions_RESTRICT_GROUP0 :: Scte35DeviceRestrictions
pattern Scte35DeviceRestrictions_RESTRICT_GROUP0 = Scte35DeviceRestrictions' "RESTRICT_GROUP0"

pattern Scte35DeviceRestrictions_RESTRICT_GROUP1 :: Scte35DeviceRestrictions
pattern Scte35DeviceRestrictions_RESTRICT_GROUP1 = Scte35DeviceRestrictions' "RESTRICT_GROUP1"

pattern Scte35DeviceRestrictions_RESTRICT_GROUP2 :: Scte35DeviceRestrictions
pattern Scte35DeviceRestrictions_RESTRICT_GROUP2 = Scte35DeviceRestrictions' "RESTRICT_GROUP2"

{-# COMPLETE
  Scte35DeviceRestrictions_NONE,
  Scte35DeviceRestrictions_RESTRICT_GROUP0,
  Scte35DeviceRestrictions_RESTRICT_GROUP1,
  Scte35DeviceRestrictions_RESTRICT_GROUP2,
  Scte35DeviceRestrictions'
  #-}
