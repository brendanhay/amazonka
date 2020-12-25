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
        PartOfSpeechTagTypeAdj,
        PartOfSpeechTagTypeAdp,
        PartOfSpeechTagTypeAdv,
        PartOfSpeechTagTypeAux,
        PartOfSpeechTagTypeConj,
        PartOfSpeechTagTypeCconj,
        PartOfSpeechTagTypeDet,
        PartOfSpeechTagTypeIntj,
        PartOfSpeechTagTypeNoun,
        PartOfSpeechTagTypeNum,
        PartOfSpeechTagTypeO,
        PartOfSpeechTagTypePart,
        PartOfSpeechTagTypePron,
        PartOfSpeechTagTypePropn,
        PartOfSpeechTagTypePunct,
        PartOfSpeechTagTypeSconj,
        PartOfSpeechTagTypeSym,
        PartOfSpeechTagTypeVerb,
        fromPartOfSpeechTagType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PartOfSpeechTagType = PartOfSpeechTagType'
  { fromPartOfSpeechTagType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern PartOfSpeechTagTypeAdj :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeAdj = PartOfSpeechTagType' "ADJ"

pattern PartOfSpeechTagTypeAdp :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeAdp = PartOfSpeechTagType' "ADP"

pattern PartOfSpeechTagTypeAdv :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeAdv = PartOfSpeechTagType' "ADV"

pattern PartOfSpeechTagTypeAux :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeAux = PartOfSpeechTagType' "AUX"

pattern PartOfSpeechTagTypeConj :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeConj = PartOfSpeechTagType' "CONJ"

pattern PartOfSpeechTagTypeCconj :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeCconj = PartOfSpeechTagType' "CCONJ"

pattern PartOfSpeechTagTypeDet :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeDet = PartOfSpeechTagType' "DET"

pattern PartOfSpeechTagTypeIntj :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeIntj = PartOfSpeechTagType' "INTJ"

pattern PartOfSpeechTagTypeNoun :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeNoun = PartOfSpeechTagType' "NOUN"

pattern PartOfSpeechTagTypeNum :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeNum = PartOfSpeechTagType' "NUM"

pattern PartOfSpeechTagTypeO :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeO = PartOfSpeechTagType' "O"

pattern PartOfSpeechTagTypePart :: PartOfSpeechTagType
pattern PartOfSpeechTagTypePart = PartOfSpeechTagType' "PART"

pattern PartOfSpeechTagTypePron :: PartOfSpeechTagType
pattern PartOfSpeechTagTypePron = PartOfSpeechTagType' "PRON"

pattern PartOfSpeechTagTypePropn :: PartOfSpeechTagType
pattern PartOfSpeechTagTypePropn = PartOfSpeechTagType' "PROPN"

pattern PartOfSpeechTagTypePunct :: PartOfSpeechTagType
pattern PartOfSpeechTagTypePunct = PartOfSpeechTagType' "PUNCT"

pattern PartOfSpeechTagTypeSconj :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeSconj = PartOfSpeechTagType' "SCONJ"

pattern PartOfSpeechTagTypeSym :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeSym = PartOfSpeechTagType' "SYM"

pattern PartOfSpeechTagTypeVerb :: PartOfSpeechTagType
pattern PartOfSpeechTagTypeVerb = PartOfSpeechTagType' "VERB"

{-# COMPLETE
  PartOfSpeechTagTypeAdj,
  PartOfSpeechTagTypeAdp,
  PartOfSpeechTagTypeAdv,
  PartOfSpeechTagTypeAux,
  PartOfSpeechTagTypeConj,
  PartOfSpeechTagTypeCconj,
  PartOfSpeechTagTypeDet,
  PartOfSpeechTagTypeIntj,
  PartOfSpeechTagTypeNoun,
  PartOfSpeechTagTypeNum,
  PartOfSpeechTagTypeO,
  PartOfSpeechTagTypePart,
  PartOfSpeechTagTypePron,
  PartOfSpeechTagTypePropn,
  PartOfSpeechTagTypePunct,
  PartOfSpeechTagTypeSconj,
  PartOfSpeechTagTypeSym,
  PartOfSpeechTagTypeVerb,
  PartOfSpeechTagType'
  #-}
