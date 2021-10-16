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
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode
  ( SmoothGroupTimestampOffsetMode
      ( ..,
        SmoothGroupTimestampOffsetMode_USE_CONFIGURED_OFFSET,
        SmoothGroupTimestampOffsetMode_USE_EVENT_START_DATE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Smooth Group Timestamp Offset Mode
newtype SmoothGroupTimestampOffsetMode = SmoothGroupTimestampOffsetMode'
  { fromSmoothGroupTimestampOffsetMode ::
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

pattern SmoothGroupTimestampOffsetMode_USE_CONFIGURED_OFFSET :: SmoothGroupTimestampOffsetMode
pattern SmoothGroupTimestampOffsetMode_USE_CONFIGURED_OFFSET = SmoothGroupTimestampOffsetMode' "USE_CONFIGURED_OFFSET"

pattern SmoothGroupTimestampOffsetMode_USE_EVENT_START_DATE :: SmoothGroupTimestampOffsetMode
pattern SmoothGroupTimestampOffsetMode_USE_EVENT_START_DATE = SmoothGroupTimestampOffsetMode' "USE_EVENT_START_DATE"

{-# COMPLETE
  SmoothGroupTimestampOffsetMode_USE_CONFIGURED_OFFSET,
  SmoothGroupTimestampOffsetMode_USE_EVENT_START_DATE,
  SmoothGroupTimestampOffsetMode'
  #-}
