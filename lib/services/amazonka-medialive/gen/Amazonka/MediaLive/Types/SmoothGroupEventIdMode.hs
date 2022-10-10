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
-- Module      : Amazonka.MediaLive.Types.SmoothGroupEventIdMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.SmoothGroupEventIdMode
  ( SmoothGroupEventIdMode
      ( ..,
        SmoothGroupEventIdMode_NO_EVENT_ID,
        SmoothGroupEventIdMode_USE_CONFIGURED,
        SmoothGroupEventIdMode_USE_TIMESTAMP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Smooth Group Event Id Mode
newtype SmoothGroupEventIdMode = SmoothGroupEventIdMode'
  { fromSmoothGroupEventIdMode ::
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

pattern SmoothGroupEventIdMode_NO_EVENT_ID :: SmoothGroupEventIdMode
pattern SmoothGroupEventIdMode_NO_EVENT_ID = SmoothGroupEventIdMode' "NO_EVENT_ID"

pattern SmoothGroupEventIdMode_USE_CONFIGURED :: SmoothGroupEventIdMode
pattern SmoothGroupEventIdMode_USE_CONFIGURED = SmoothGroupEventIdMode' "USE_CONFIGURED"

pattern SmoothGroupEventIdMode_USE_TIMESTAMP :: SmoothGroupEventIdMode
pattern SmoothGroupEventIdMode_USE_TIMESTAMP = SmoothGroupEventIdMode' "USE_TIMESTAMP"

{-# COMPLETE
  SmoothGroupEventIdMode_NO_EVENT_ID,
  SmoothGroupEventIdMode_USE_CONFIGURED,
  SmoothGroupEventIdMode_USE_TIMESTAMP,
  SmoothGroupEventIdMode'
  #-}
