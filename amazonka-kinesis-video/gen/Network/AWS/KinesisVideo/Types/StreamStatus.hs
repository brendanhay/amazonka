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
-- Module      : Network.AWS.KinesisVideo.Types.StreamStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.StreamStatus
  ( StreamStatus
      ( ..,
        StreamStatus_ACTIVE,
        StreamStatus_CREATING,
        StreamStatus_DELETING,
        StreamStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StreamStatus = StreamStatus'
  { fromStreamStatus ::
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

pattern StreamStatus_ACTIVE :: StreamStatus
pattern StreamStatus_ACTIVE = StreamStatus' "ACTIVE"

pattern StreamStatus_CREATING :: StreamStatus
pattern StreamStatus_CREATING = StreamStatus' "CREATING"

pattern StreamStatus_DELETING :: StreamStatus
pattern StreamStatus_DELETING = StreamStatus' "DELETING"

pattern StreamStatus_UPDATING :: StreamStatus
pattern StreamStatus_UPDATING = StreamStatus' "UPDATING"

{-# COMPLETE
  StreamStatus_ACTIVE,
  StreamStatus_CREATING,
  StreamStatus_DELETING,
  StreamStatus_UPDATING,
  StreamStatus'
  #-}
