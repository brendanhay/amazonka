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
-- Module      : Amazonka.MachineLearning.Types.RealtimeEndpointStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.RealtimeEndpointStatus
  ( RealtimeEndpointStatus
      ( ..,
        RealtimeEndpointStatus_FAILED,
        RealtimeEndpointStatus_NONE,
        RealtimeEndpointStatus_READY,
        RealtimeEndpointStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RealtimeEndpointStatus = RealtimeEndpointStatus'
  { fromRealtimeEndpointStatus ::
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
