{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ReprocessingStatus = ReprocessingStatus'
  { fromReprocessingStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
