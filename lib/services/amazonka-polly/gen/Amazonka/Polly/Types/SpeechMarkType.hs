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
-- Module      : Amazonka.Polly.Types.SpeechMarkType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Types.SpeechMarkType
  ( SpeechMarkType
      ( ..,
        SpeechMarkType_Sentence,
        SpeechMarkType_Ssml,
        SpeechMarkType_Viseme,
        SpeechMarkType_Word
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SpeechMarkType = SpeechMarkType'
  { fromSpeechMarkType ::
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

pattern SpeechMarkType_Sentence :: SpeechMarkType
pattern SpeechMarkType_Sentence = SpeechMarkType' "sentence"

pattern SpeechMarkType_Ssml :: SpeechMarkType
pattern SpeechMarkType_Ssml = SpeechMarkType' "ssml"

pattern SpeechMarkType_Viseme :: SpeechMarkType
pattern SpeechMarkType_Viseme = SpeechMarkType' "viseme"

pattern SpeechMarkType_Word :: SpeechMarkType
pattern SpeechMarkType_Word = SpeechMarkType' "word"

{-# COMPLETE
  SpeechMarkType_Sentence,
  SpeechMarkType_Ssml,
  SpeechMarkType_Viseme,
  SpeechMarkType_Word,
  SpeechMarkType'
  #-}
