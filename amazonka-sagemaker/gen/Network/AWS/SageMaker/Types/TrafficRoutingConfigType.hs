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
-- Module      : Network.AWS.SageMaker.Types.TrafficRoutingConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrafficRoutingConfigType
  ( TrafficRoutingConfigType
      ( ..,
        TrafficRoutingConfigType_ALL_AT_ONCE,
        TrafficRoutingConfigType_CANARY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TrafficRoutingConfigType = TrafficRoutingConfigType'
  { fromTrafficRoutingConfigType ::
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

pattern TrafficRoutingConfigType_ALL_AT_ONCE :: TrafficRoutingConfigType
pattern TrafficRoutingConfigType_ALL_AT_ONCE = TrafficRoutingConfigType' "ALL_AT_ONCE"

pattern TrafficRoutingConfigType_CANARY :: TrafficRoutingConfigType
pattern TrafficRoutingConfigType_CANARY = TrafficRoutingConfigType' "CANARY"

{-# COMPLETE
  TrafficRoutingConfigType_ALL_AT_ONCE,
  TrafficRoutingConfigType_CANARY,
  TrafficRoutingConfigType'
  #-}
