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
-- Module      : Amazonka.AWSHealth.Types.EventStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EventStatusCode
  ( EventStatusCode
      ( ..,
        EventStatusCode_Closed,
        EventStatusCode_Open,
        EventStatusCode_Upcoming
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EventStatusCode = EventStatusCode'
  { fromEventStatusCode ::
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
