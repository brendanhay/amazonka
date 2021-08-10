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
-- Module      : Network.AWS.Pinpoint.Types.Frequency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Frequency
  ( Frequency
      ( ..,
        Frequency_DAILY,
        Frequency_EVENT,
        Frequency_HOURLY,
        Frequency_MONTHLY,
        Frequency_ONCE,
        Frequency_WEEKLY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype Frequency = Frequency'
  { fromFrequency ::
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

pattern Frequency_DAILY :: Frequency
pattern Frequency_DAILY = Frequency' "DAILY"

pattern Frequency_EVENT :: Frequency
pattern Frequency_EVENT = Frequency' "EVENT"

pattern Frequency_HOURLY :: Frequency
pattern Frequency_HOURLY = Frequency' "HOURLY"

pattern Frequency_MONTHLY :: Frequency
pattern Frequency_MONTHLY = Frequency' "MONTHLY"

pattern Frequency_ONCE :: Frequency
pattern Frequency_ONCE = Frequency' "ONCE"

pattern Frequency_WEEKLY :: Frequency
pattern Frequency_WEEKLY = Frequency' "WEEKLY"

{-# COMPLETE
  Frequency_DAILY,
  Frequency_EVENT,
  Frequency_HOURLY,
  Frequency_MONTHLY,
  Frequency_ONCE,
  Frequency_WEEKLY,
  Frequency'
  #-}
