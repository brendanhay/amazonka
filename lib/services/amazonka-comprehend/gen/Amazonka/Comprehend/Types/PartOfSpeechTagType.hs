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
-- Module      : Amazonka.Comprehend.Types.PartOfSpeechTagType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.PartOfSpeechTagType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PartOfSpeechTagType = PartOfSpeechTagType'
  { fromPartOfSpeechTagType ::
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
