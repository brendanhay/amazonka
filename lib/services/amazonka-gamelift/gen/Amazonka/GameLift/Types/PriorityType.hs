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
-- Module      : Amazonka.GameLift.Types.PriorityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.PriorityType
  ( PriorityType
      ( ..,
        PriorityType_COST,
        PriorityType_DESTINATION,
        PriorityType_LATENCY,
        PriorityType_LOCATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PriorityType = PriorityType'
  { fromPriorityType ::
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

pattern PriorityType_COST :: PriorityType
pattern PriorityType_COST = PriorityType' "COST"

pattern PriorityType_DESTINATION :: PriorityType
pattern PriorityType_DESTINATION = PriorityType' "DESTINATION"

pattern PriorityType_LATENCY :: PriorityType
pattern PriorityType_LATENCY = PriorityType' "LATENCY"

pattern PriorityType_LOCATION :: PriorityType
pattern PriorityType_LOCATION = PriorityType' "LOCATION"

{-# COMPLETE
  PriorityType_COST,
  PriorityType_DESTINATION,
  PriorityType_LATENCY,
  PriorityType_LOCATION,
  PriorityType'
  #-}
