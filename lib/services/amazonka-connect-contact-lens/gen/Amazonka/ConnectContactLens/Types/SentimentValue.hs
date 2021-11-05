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
-- Module      : Amazonka.ConnectContactLens.Types.SentimentValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectContactLens.Types.SentimentValue
  ( SentimentValue
      ( ..,
        SentimentValue_NEGATIVE,
        SentimentValue_NEUTRAL,
        SentimentValue_POSITIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SentimentValue = SentimentValue'
  { fromSentimentValue ::
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

pattern SentimentValue_NEGATIVE :: SentimentValue
pattern SentimentValue_NEGATIVE = SentimentValue' "NEGATIVE"

pattern SentimentValue_NEUTRAL :: SentimentValue
pattern SentimentValue_NEUTRAL = SentimentValue' "NEUTRAL"

pattern SentimentValue_POSITIVE :: SentimentValue
pattern SentimentValue_POSITIVE = SentimentValue' "POSITIVE"

{-# COMPLETE
  SentimentValue_NEGATIVE,
  SentimentValue_NEUTRAL,
  SentimentValue_POSITIVE,
  SentimentValue'
  #-}
