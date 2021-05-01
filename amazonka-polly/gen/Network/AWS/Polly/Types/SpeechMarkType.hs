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
-- Module      : Network.AWS.Polly.Types.SpeechMarkType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.SpeechMarkType
  ( SpeechMarkType
      ( ..,
        SpeechMarkType_Sentence,
        SpeechMarkType_Ssml,
        SpeechMarkType_Viseme,
        SpeechMarkType_Word
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SpeechMarkType = SpeechMarkType'
  { fromSpeechMarkType ::
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
