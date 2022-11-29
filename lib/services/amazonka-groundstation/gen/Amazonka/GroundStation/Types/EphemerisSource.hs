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
-- Module      : Amazonka.GroundStation.Types.EphemerisSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EphemerisSource
  ( EphemerisSource
      ( ..,
        EphemerisSource_CUSTOMER_PROVIDED,
        EphemerisSource_SPACE_TRACK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EphemerisSource = EphemerisSource'
  { fromEphemerisSource ::
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

pattern EphemerisSource_CUSTOMER_PROVIDED :: EphemerisSource
pattern EphemerisSource_CUSTOMER_PROVIDED = EphemerisSource' "CUSTOMER_PROVIDED"

pattern EphemerisSource_SPACE_TRACK :: EphemerisSource
pattern EphemerisSource_SPACE_TRACK = EphemerisSource' "SPACE_TRACK"

{-# COMPLETE
  EphemerisSource_CUSTOMER_PROVIDED,
  EphemerisSource_SPACE_TRACK,
  EphemerisSource'
  #-}
