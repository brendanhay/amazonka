{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PartOfSpeechTagType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PartOfSpeechTagType
  ( PartOfSpeechTagType
      ( PartOfSpeechTagType',
        Adj,
        Adp,
        Adv,
        Aux,
        Cconj,
        Conj,
        Det,
        Intj,
        Noun,
        Num,
        O,
        Part,
        Pron,
        Propn,
        Punct,
        Sconj,
        Sym,
        Verb
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PartOfSpeechTagType = PartOfSpeechTagType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Adj :: PartOfSpeechTagType
pattern Adj = PartOfSpeechTagType' "ADJ"

pattern Adp :: PartOfSpeechTagType
pattern Adp = PartOfSpeechTagType' "ADP"

pattern Adv :: PartOfSpeechTagType
pattern Adv = PartOfSpeechTagType' "ADV"

pattern Aux :: PartOfSpeechTagType
pattern Aux = PartOfSpeechTagType' "AUX"

pattern Cconj :: PartOfSpeechTagType
pattern Cconj = PartOfSpeechTagType' "CCONJ"

pattern Conj :: PartOfSpeechTagType
pattern Conj = PartOfSpeechTagType' "CONJ"

pattern Det :: PartOfSpeechTagType
pattern Det = PartOfSpeechTagType' "DET"

pattern Intj :: PartOfSpeechTagType
pattern Intj = PartOfSpeechTagType' "INTJ"

pattern Noun :: PartOfSpeechTagType
pattern Noun = PartOfSpeechTagType' "NOUN"

pattern Num :: PartOfSpeechTagType
pattern Num = PartOfSpeechTagType' "NUM"

pattern O :: PartOfSpeechTagType
pattern O = PartOfSpeechTagType' "O"

pattern Part :: PartOfSpeechTagType
pattern Part = PartOfSpeechTagType' "PART"

pattern Pron :: PartOfSpeechTagType
pattern Pron = PartOfSpeechTagType' "PRON"

pattern Propn :: PartOfSpeechTagType
pattern Propn = PartOfSpeechTagType' "PROPN"

pattern Punct :: PartOfSpeechTagType
pattern Punct = PartOfSpeechTagType' "PUNCT"

pattern Sconj :: PartOfSpeechTagType
pattern Sconj = PartOfSpeechTagType' "SCONJ"

pattern Sym :: PartOfSpeechTagType
pattern Sym = PartOfSpeechTagType' "SYM"

pattern Verb :: PartOfSpeechTagType
pattern Verb = PartOfSpeechTagType' "VERB"

{-# COMPLETE
  Adj,
  Adp,
  Adv,
  Aux,
  Cconj,
  Conj,
  Det,
  Intj,
  Noun,
  Num,
  O,
  Part,
  Pron,
  Propn,
  Punct,
  Sconj,
  Sym,
  Verb,
  PartOfSpeechTagType'
  #-}
