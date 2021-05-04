{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentType
  ( SentimentType
      ( ..,
        SentimentType_MIXED,
        SentimentType_NEGATIVE,
        SentimentType_NEUTRAL,
        SentimentType_POSITIVE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SentimentType = SentimentType'
  { fromSentimentType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
