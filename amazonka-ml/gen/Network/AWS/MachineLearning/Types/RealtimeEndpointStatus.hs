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
-- Module      : Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
  ( RealtimeEndpointStatus
      ( ..,
        RealtimeEndpointStatus_FAILED,
        RealtimeEndpointStatus_NONE,
        RealtimeEndpointStatus_READY,
        RealtimeEndpointStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RealtimeEndpointStatus = RealtimeEndpointStatus'
  { fromRealtimeEndpointStatus ::
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

pattern RealtimeEndpointStatus_FAILED :: RealtimeEndpointStatus
pattern RealtimeEndpointStatus_FAILED = RealtimeEndpointStatus' "FAILED"

pattern RealtimeEndpointStatus_NONE :: RealtimeEndpointStatus
pattern RealtimeEndpointStatus_NONE = RealtimeEndpointStatus' "NONE"

pattern RealtimeEndpointStatus_READY :: RealtimeEndpointStatus
pattern RealtimeEndpointStatus_READY = RealtimeEndpointStatus' "READY"

pattern RealtimeEndpointStatus_UPDATING :: RealtimeEndpointStatus
pattern RealtimeEndpointStatus_UPDATING = RealtimeEndpointStatus' "UPDATING"

{-# COMPLETE
  RealtimeEndpointStatus_FAILED,
  RealtimeEndpointStatus_NONE,
  RealtimeEndpointStatus_READY,
  RealtimeEndpointStatus_UPDATING,
  RealtimeEndpointStatus'
  #-}
