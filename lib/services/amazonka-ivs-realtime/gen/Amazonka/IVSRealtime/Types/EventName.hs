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
-- Module      : Amazonka.IVSRealtime.Types.EventName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.EventName
  ( EventName
      ( ..,
        EventName_JOINED,
        EventName_JOIN_ERROR,
        EventName_LEFT,
        EventName_PUBLISH_ERROR,
        EventName_PUBLISH_STARTED,
        EventName_PUBLISH_STOPPED,
        EventName_SUBSCRIBE_ERROR,
        EventName_SUBSCRIBE_STARTED,
        EventName_SUBSCRIBE_STOPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventName = EventName'
  { fromEventName ::
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

pattern EventName_JOINED :: EventName
pattern EventName_JOINED = EventName' "JOINED"

pattern EventName_JOIN_ERROR :: EventName
pattern EventName_JOIN_ERROR = EventName' "JOIN_ERROR"

pattern EventName_LEFT :: EventName
pattern EventName_LEFT = EventName' "LEFT"

pattern EventName_PUBLISH_ERROR :: EventName
pattern EventName_PUBLISH_ERROR = EventName' "PUBLISH_ERROR"

pattern EventName_PUBLISH_STARTED :: EventName
pattern EventName_PUBLISH_STARTED = EventName' "PUBLISH_STARTED"

pattern EventName_PUBLISH_STOPPED :: EventName
pattern EventName_PUBLISH_STOPPED = EventName' "PUBLISH_STOPPED"

pattern EventName_SUBSCRIBE_ERROR :: EventName
pattern EventName_SUBSCRIBE_ERROR = EventName' "SUBSCRIBE_ERROR"

pattern EventName_SUBSCRIBE_STARTED :: EventName
pattern EventName_SUBSCRIBE_STARTED = EventName' "SUBSCRIBE_STARTED"

pattern EventName_SUBSCRIBE_STOPPED :: EventName
pattern EventName_SUBSCRIBE_STOPPED = EventName' "SUBSCRIBE_STOPPED"

{-# COMPLETE
  EventName_JOINED,
  EventName_JOIN_ERROR,
  EventName_LEFT,
  EventName_PUBLISH_ERROR,
  EventName_PUBLISH_STARTED,
  EventName_PUBLISH_STOPPED,
  EventName_SUBSCRIBE_ERROR,
  EventName_SUBSCRIBE_STARTED,
  EventName_SUBSCRIBE_STOPPED,
  EventName'
  #-}
