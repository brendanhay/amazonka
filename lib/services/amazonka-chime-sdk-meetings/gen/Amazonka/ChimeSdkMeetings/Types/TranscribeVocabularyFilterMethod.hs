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
-- Module      : Amazonka.ChimeSdkMeetings.Types.TranscribeVocabularyFilterMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.TranscribeVocabularyFilterMethod
  ( TranscribeVocabularyFilterMethod
      ( ..,
        TranscribeVocabularyFilterMethod_Mask,
        TranscribeVocabularyFilterMethod_Remove,
        TranscribeVocabularyFilterMethod_Tag
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TranscribeVocabularyFilterMethod = TranscribeVocabularyFilterMethod'
  { fromTranscribeVocabularyFilterMethod ::
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

pattern TranscribeVocabularyFilterMethod_Mask :: TranscribeVocabularyFilterMethod
pattern TranscribeVocabularyFilterMethod_Mask = TranscribeVocabularyFilterMethod' "mask"

pattern TranscribeVocabularyFilterMethod_Remove :: TranscribeVocabularyFilterMethod
pattern TranscribeVocabularyFilterMethod_Remove = TranscribeVocabularyFilterMethod' "remove"

pattern TranscribeVocabularyFilterMethod_Tag :: TranscribeVocabularyFilterMethod
pattern TranscribeVocabularyFilterMethod_Tag = TranscribeVocabularyFilterMethod' "tag"

{-# COMPLETE
  TranscribeVocabularyFilterMethod_Mask,
  TranscribeVocabularyFilterMethod_Remove,
  TranscribeVocabularyFilterMethod_Tag,
  TranscribeVocabularyFilterMethod'
  #-}
