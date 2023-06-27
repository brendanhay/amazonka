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
-- Module      : Amazonka.QuickSight.Types.TopicTimeGranularity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicTimeGranularity
  ( TopicTimeGranularity
      ( ..,
        TopicTimeGranularity_DAY,
        TopicTimeGranularity_HOUR,
        TopicTimeGranularity_MINUTE,
        TopicTimeGranularity_MONTH,
        TopicTimeGranularity_QUARTER,
        TopicTimeGranularity_SECOND,
        TopicTimeGranularity_WEEK,
        TopicTimeGranularity_YEAR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TopicTimeGranularity = TopicTimeGranularity'
  { fromTopicTimeGranularity ::
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

pattern TopicTimeGranularity_DAY :: TopicTimeGranularity
pattern TopicTimeGranularity_DAY = TopicTimeGranularity' "DAY"

pattern TopicTimeGranularity_HOUR :: TopicTimeGranularity
pattern TopicTimeGranularity_HOUR = TopicTimeGranularity' "HOUR"

pattern TopicTimeGranularity_MINUTE :: TopicTimeGranularity
pattern TopicTimeGranularity_MINUTE = TopicTimeGranularity' "MINUTE"

pattern TopicTimeGranularity_MONTH :: TopicTimeGranularity
pattern TopicTimeGranularity_MONTH = TopicTimeGranularity' "MONTH"

pattern TopicTimeGranularity_QUARTER :: TopicTimeGranularity
pattern TopicTimeGranularity_QUARTER = TopicTimeGranularity' "QUARTER"

pattern TopicTimeGranularity_SECOND :: TopicTimeGranularity
pattern TopicTimeGranularity_SECOND = TopicTimeGranularity' "SECOND"

pattern TopicTimeGranularity_WEEK :: TopicTimeGranularity
pattern TopicTimeGranularity_WEEK = TopicTimeGranularity' "WEEK"

pattern TopicTimeGranularity_YEAR :: TopicTimeGranularity
pattern TopicTimeGranularity_YEAR = TopicTimeGranularity' "YEAR"

{-# COMPLETE
  TopicTimeGranularity_DAY,
  TopicTimeGranularity_HOUR,
  TopicTimeGranularity_MINUTE,
  TopicTimeGranularity_MONTH,
  TopicTimeGranularity_QUARTER,
  TopicTimeGranularity_SECOND,
  TopicTimeGranularity_WEEK,
  TopicTimeGranularity_YEAR,
  TopicTimeGranularity'
  #-}
