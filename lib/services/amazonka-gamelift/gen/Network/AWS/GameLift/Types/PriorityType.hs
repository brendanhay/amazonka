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
-- Module      : Network.AWS.GameLift.Types.PriorityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PriorityType
  ( PriorityType
      ( ..,
        PriorityType_COST,
        PriorityType_DESTINATION,
        PriorityType_LATENCY,
        PriorityType_LOCATION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PriorityType = PriorityType'
  { fromPriorityType ::
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
