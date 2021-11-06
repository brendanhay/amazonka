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
-- Module      : Amazonka.Comprehend.Types.SentimentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.SentimentType
  ( SentimentType
      ( ..,
        SentimentType_MIXED,
        SentimentType_NEGATIVE,
        SentimentType_NEUTRAL,
        SentimentType_POSITIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SentimentType = SentimentType'
  { fromSentimentType ::
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

pattern SentimentType_MIXED :: SentimentType
pattern SentimentType_MIXED = SentimentType' "MIXED"

pattern SentimentType_NEGATIVE :: SentimentType
pattern SentimentType_NEGATIVE = SentimentType' "NEGATIVE"

pattern SentimentType_NEUTRAL :: SentimentType
pattern SentimentType_NEUTRAL = SentimentType' "NEUTRAL"

pattern SentimentType_POSITIVE :: SentimentType
pattern SentimentType_POSITIVE = SentimentType' "POSITIVE"

{-# COMPLETE
  SentimentType_MIXED,
  SentimentType_NEGATIVE,
  SentimentType_NEUTRAL,
  SentimentType_POSITIVE,
  SentimentType'
  #-}
