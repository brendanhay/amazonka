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
-- Module      : Network.AWS.Comprehend.Types.PartOfSpeechTagType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PartOfSpeechTagType
  ( PartOfSpeechTagType
      ( ..,
        PartOfSpeechTagType_ADJ,
        PartOfSpeechTagType_ADP,
        PartOfSpeechTagType_ADV,
        PartOfSpeechTagType_AUX,
        PartOfSpeechTagType_CCONJ,
        PartOfSpeechTagType_CONJ,
        PartOfSpeechTagType_DET,
        PartOfSpeechTagType_INTJ,
        PartOfSpeechTagType_NOUN,
        PartOfSpeechTagType_NUM,
        PartOfSpeechTagType_O,
        PartOfSpeechTagType_PART,
        PartOfSpeechTagType_PRON,
        PartOfSpeechTagType_PROPN,
        PartOfSpeechTagType_PUNCT,
        PartOfSpeechTagType_SCONJ,
        PartOfSpeechTagType_SYM,
        PartOfSpeechTagType_VERB
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype PartOfSpeechTagType = PartOfSpeechTagType'
  { fromPartOfSpeechTagType ::
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

pattern PartOfSpeechTagType_ADJ :: PartOfSpeechTagType
pattern PartOfSpeechTagType_ADJ = PartOfSpeechTagType' "ADJ"

pattern PartOfSpeechTagType_ADP :: PartOfSpeechTagType
pattern PartOfSpeechTagType_ADP = PartOfSpeechTagType' "ADP"

pattern PartOfSpeechTagType_ADV :: PartOfSpeechTagType
pattern PartOfSpeechTagType_ADV = PartOfSpeechTagType' "ADV"

pattern PartOfSpeechTagType_AUX :: PartOfSpeechTagType
pattern PartOfSpeechTagType_AUX = PartOfSpeechTagType' "AUX"

pattern PartOfSpeechTagType_CCONJ :: PartOfSpeechTagType
pattern PartOfSpeechTagType_CCONJ = PartOfSpeechTagType' "CCONJ"

pattern PartOfSpeechTagType_CONJ :: PartOfSpeechTagType
pattern PartOfSpeechTagType_CONJ = PartOfSpeechTagType' "CONJ"

pattern PartOfSpeechTagType_DET :: PartOfSpeechTagType
pattern PartOfSpeechTagType_DET = PartOfSpeechTagType' "DET"

pattern PartOfSpeechTagType_INTJ :: PartOfSpeechTagType
pattern PartOfSpeechTagType_INTJ = PartOfSpeechTagType' "INTJ"

pattern PartOfSpeechTagType_NOUN :: PartOfSpeechTagType
pattern PartOfSpeechTagType_NOUN = PartOfSpeechTagType' "NOUN"

pattern PartOfSpeechTagType_NUM :: PartOfSpeechTagType
pattern PartOfSpeechTagType_NUM = PartOfSpeechTagType' "NUM"

pattern PartOfSpeechTagType_O :: PartOfSpeechTagType
pattern PartOfSpeechTagType_O = PartOfSpeechTagType' "O"

pattern PartOfSpeechTagType_PART :: PartOfSpeechTagType
pattern PartOfSpeechTagType_PART = PartOfSpeechTagType' "PART"

pattern PartOfSpeechTagType_PRON :: PartOfSpeechTagType
pattern PartOfSpeechTagType_PRON = PartOfSpeechTagType' "PRON"

pattern PartOfSpeechTagType_PROPN :: PartOfSpeechTagType
pattern PartOfSpeechTagType_PROPN = PartOfSpeechTagType' "PROPN"

pattern PartOfSpeechTagType_PUNCT :: PartOfSpeechTagType
pattern PartOfSpeechTagType_PUNCT = PartOfSpeechTagType' "PUNCT"

pattern PartOfSpeechTagType_SCONJ :: PartOfSpeechTagType
pattern PartOfSpeechTagType_SCONJ = PartOfSpeechTagType' "SCONJ"

pattern PartOfSpeechTagType_SYM :: PartOfSpeechTagType
pattern PartOfSpeechTagType_SYM = PartOfSpeechTagType' "SYM"

pattern PartOfSpeechTagType_VERB :: PartOfSpeechTagType
pattern PartOfSpeechTagType_VERB = PartOfSpeechTagType' "VERB"

{-# COMPLETE
  PartOfSpeechTagType_ADJ,
  PartOfSpeechTagType_ADP,
  PartOfSpeechTagType_ADV,
  PartOfSpeechTagType_AUX,
  PartOfSpeechTagType_CCONJ,
  PartOfSpeechTagType_CONJ,
  PartOfSpeechTagType_DET,
  PartOfSpeechTagType_INTJ,
  PartOfSpeechTagType_NOUN,
  PartOfSpeechTagType_NUM,
  PartOfSpeechTagType_O,
  PartOfSpeechTagType_PART,
  PartOfSpeechTagType_PRON,
  PartOfSpeechTagType_PROPN,
  PartOfSpeechTagType_PUNCT,
  PartOfSpeechTagType_SCONJ,
  PartOfSpeechTagType_SYM,
  PartOfSpeechTagType_VERB,
  PartOfSpeechTagType'
  #-}
