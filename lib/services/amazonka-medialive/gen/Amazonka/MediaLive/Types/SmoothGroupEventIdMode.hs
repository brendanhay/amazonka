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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Smooth Group Event Id Mode
newtype SmoothGroupEventIdMode = SmoothGroupEventIdMode'
  { fromSmoothGroupEventIdMode ::
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
