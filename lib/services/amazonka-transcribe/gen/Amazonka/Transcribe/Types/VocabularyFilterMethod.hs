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
-- Module      : Amazonka.Transcribe.Types.VocabularyFilterMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.VocabularyFilterMethod
  ( VocabularyFilterMethod
      ( ..,
        VocabularyFilterMethod_Mask,
        VocabularyFilterMethod_Remove,
        VocabularyFilterMethod_Tag
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VocabularyFilterMethod = VocabularyFilterMethod'
  { fromVocabularyFilterMethod ::
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

pattern VocabularyFilterMethod_Mask :: VocabularyFilterMethod
pattern VocabularyFilterMethod_Mask = VocabularyFilterMethod' "mask"

pattern VocabularyFilterMethod_Remove :: VocabularyFilterMethod
pattern VocabularyFilterMethod_Remove = VocabularyFilterMethod' "remove"

pattern VocabularyFilterMethod_Tag :: VocabularyFilterMethod
pattern VocabularyFilterMethod_Tag = VocabularyFilterMethod' "tag"

{-# COMPLETE
  VocabularyFilterMethod_Mask,
  VocabularyFilterMethod_Remove,
  VocabularyFilterMethod_Tag,
  VocabularyFilterMethod'
  #-}
