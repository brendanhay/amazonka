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
-- Module      : Network.AWS.AWSHealth.Types.EventStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventStatusCode
  ( EventStatusCode
      ( ..,
        EventStatusCode_Closed,
        EventStatusCode_Open,
        EventStatusCode_Upcoming
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EventStatusCode = EventStatusCode'
  { fromEventStatusCode ::
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

pattern EventStatusCode_Closed :: EventStatusCode
pattern EventStatusCode_Closed = EventStatusCode' "closed"

pattern EventStatusCode_Open :: EventStatusCode
pattern EventStatusCode_Open = EventStatusCode' "open"

pattern EventStatusCode_Upcoming :: EventStatusCode
pattern EventStatusCode_Upcoming = EventStatusCode' "upcoming"

{-# COMPLETE
  EventStatusCode_Closed,
  EventStatusCode_Open,
  EventStatusCode_Upcoming,
  EventStatusCode'
  #-}
