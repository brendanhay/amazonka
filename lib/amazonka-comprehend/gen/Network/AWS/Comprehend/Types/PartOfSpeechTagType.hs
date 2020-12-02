{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PartOfSpeechTagType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PartOfSpeechTagType where

import Network.AWS.Prelude

data PartOfSpeechTagType
  = Adj
  | Adp
  | Adv
  | Aux
  | Cconj
  | Conj
  | Det
  | Intj
  | Noun
  | Num
  | O
  | Part
  | Pron
  | Propn
  | Punct
  | Sconj
  | Sym
  | Verb
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText PartOfSpeechTagType where
  parser =
    takeLowerText >>= \case
      "adj" -> pure Adj
      "adp" -> pure Adp
      "adv" -> pure Adv
      "aux" -> pure Aux
      "cconj" -> pure Cconj
      "conj" -> pure Conj
      "det" -> pure Det
      "intj" -> pure Intj
      "noun" -> pure Noun
      "num" -> pure Num
      "o" -> pure O
      "part" -> pure Part
      "pron" -> pure Pron
      "propn" -> pure Propn
      "punct" -> pure Punct
      "sconj" -> pure Sconj
      "sym" -> pure Sym
      "verb" -> pure Verb
      e ->
        fromTextError $
          "Failure parsing PartOfSpeechTagType from value: '" <> e
            <> "'. Accepted values: adj, adp, adv, aux, cconj, conj, det, intj, noun, num, o, part, pron, propn, punct, sconj, sym, verb"

instance ToText PartOfSpeechTagType where
  toText = \case
    Adj -> "ADJ"
    Adp -> "ADP"
    Adv -> "ADV"
    Aux -> "AUX"
    Cconj -> "CCONJ"
    Conj -> "CONJ"
    Det -> "DET"
    Intj -> "INTJ"
    Noun -> "NOUN"
    Num -> "NUM"
    O -> "O"
    Part -> "PART"
    Pron -> "PRON"
    Propn -> "PROPN"
    Punct -> "PUNCT"
    Sconj -> "SCONJ"
    Sym -> "SYM"
    Verb -> "VERB"

instance Hashable PartOfSpeechTagType

instance NFData PartOfSpeechTagType

instance ToByteString PartOfSpeechTagType

instance ToQuery PartOfSpeechTagType

instance ToHeader PartOfSpeechTagType

instance FromJSON PartOfSpeechTagType where
  parseJSON = parseJSONText "PartOfSpeechTagType"
