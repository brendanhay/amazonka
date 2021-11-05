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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype VocabularyFilterMethod = VocabularyFilterMethod'
  { fromVocabularyFilterMethod ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
