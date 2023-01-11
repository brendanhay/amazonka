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
-- Module      : Amazonka.SageMaker.Types.TrafficRoutingConfigType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrafficRoutingConfigType
  ( TrafficRoutingConfigType
      ( ..,
        TrafficRoutingConfigType_ALL_AT_ONCE,
        TrafficRoutingConfigType_CANARY,
        TrafficRoutingConfigType_LINEAR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TrafficRoutingConfigType = TrafficRoutingConfigType'
  { fromTrafficRoutingConfigType ::
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

pattern TrafficRoutingConfigType_ALL_AT_ONCE :: TrafficRoutingConfigType
pattern TrafficRoutingConfigType_ALL_AT_ONCE = TrafficRoutingConfigType' "ALL_AT_ONCE"

pattern TrafficRoutingConfigType_CANARY :: TrafficRoutingConfigType
pattern TrafficRoutingConfigType_CANARY = TrafficRoutingConfigType' "CANARY"

pattern TrafficRoutingConfigType_LINEAR :: TrafficRoutingConfigType
pattern TrafficRoutingConfigType_LINEAR = TrafficRoutingConfigType' "LINEAR"

{-# COMPLETE
  TrafficRoutingConfigType_ALL_AT_ONCE,
  TrafficRoutingConfigType_CANARY,
  TrafficRoutingConfigType_LINEAR,
  TrafficRoutingConfigType'
  #-}
