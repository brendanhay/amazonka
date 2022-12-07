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
-- Module      : Amazonka.MediaLive.Types.SmoothGroupTimestampOffsetMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.SmoothGroupTimestampOffsetMode
  ( SmoothGroupTimestampOffsetMode
      ( ..,
        SmoothGroupTimestampOffsetMode_USE_CONFIGURED_OFFSET,
        SmoothGroupTimestampOffsetMode_USE_EVENT_START_DATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Smooth Group Timestamp Offset Mode
newtype SmoothGroupTimestampOffsetMode = SmoothGroupTimestampOffsetMode'
  { fromSmoothGroupTimestampOffsetMode ::
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

pattern SmoothGroupTimestampOffsetMode_USE_CONFIGURED_OFFSET :: SmoothGroupTimestampOffsetMode
pattern SmoothGroupTimestampOffsetMode_USE_CONFIGURED_OFFSET = SmoothGroupTimestampOffsetMode' "USE_CONFIGURED_OFFSET"

pattern SmoothGroupTimestampOffsetMode_USE_EVENT_START_DATE :: SmoothGroupTimestampOffsetMode
pattern SmoothGroupTimestampOffsetMode_USE_EVENT_START_DATE = SmoothGroupTimestampOffsetMode' "USE_EVENT_START_DATE"

{-# COMPLETE
  SmoothGroupTimestampOffsetMode_USE_CONFIGURED_OFFSET,
  SmoothGroupTimestampOffsetMode_USE_EVENT_START_DATE,
  SmoothGroupTimestampOffsetMode'
  #-}
