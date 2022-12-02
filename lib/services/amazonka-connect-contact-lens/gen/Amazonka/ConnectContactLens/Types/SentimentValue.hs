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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SentimentValue = SentimentValue'
  { fromSentimentValue ::
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
