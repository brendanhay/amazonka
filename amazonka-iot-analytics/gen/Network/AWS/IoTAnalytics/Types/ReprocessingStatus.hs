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
-- Module      : Network.AWS.IoTAnalytics.Types.ReprocessingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ReprocessingStatus
  ( ReprocessingStatus
      ( ..,
        ReprocessingStatus_CANCELLED,
        ReprocessingStatus_FAILED,
        ReprocessingStatus_RUNNING,
        ReprocessingStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ReprocessingStatus = ReprocessingStatus'
  { fromReprocessingStatus ::
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

pattern ReprocessingStatus_CANCELLED :: ReprocessingStatus
pattern ReprocessingStatus_CANCELLED = ReprocessingStatus' "CANCELLED"

pattern ReprocessingStatus_FAILED :: ReprocessingStatus
pattern ReprocessingStatus_FAILED = ReprocessingStatus' "FAILED"

pattern ReprocessingStatus_RUNNING :: ReprocessingStatus
pattern ReprocessingStatus_RUNNING = ReprocessingStatus' "RUNNING"

pattern ReprocessingStatus_SUCCEEDED :: ReprocessingStatus
pattern ReprocessingStatus_SUCCEEDED = ReprocessingStatus' "SUCCEEDED"

{-# COMPLETE
  ReprocessingStatus_CANCELLED,
  ReprocessingStatus_FAILED,
  ReprocessingStatus_RUNNING,
  ReprocessingStatus_SUCCEEDED,
  ReprocessingStatus'
  #-}
