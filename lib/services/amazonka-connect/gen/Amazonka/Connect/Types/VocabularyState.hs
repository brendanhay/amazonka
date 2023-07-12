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
-- Module      : Amazonka.Connect.Types.VocabularyState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.VocabularyState
  ( VocabularyState
      ( ..,
        VocabularyState_ACTIVE,
        VocabularyState_CREATION_FAILED,
        VocabularyState_CREATION_IN_PROGRESS,
        VocabularyState_DELETE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VocabularyState = VocabularyState'
  { fromVocabularyState ::
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

pattern VocabularyState_ACTIVE :: VocabularyState
pattern VocabularyState_ACTIVE = VocabularyState' "ACTIVE"

pattern VocabularyState_CREATION_FAILED :: VocabularyState
pattern VocabularyState_CREATION_FAILED = VocabularyState' "CREATION_FAILED"

pattern VocabularyState_CREATION_IN_PROGRESS :: VocabularyState
pattern VocabularyState_CREATION_IN_PROGRESS = VocabularyState' "CREATION_IN_PROGRESS"

pattern VocabularyState_DELETE_IN_PROGRESS :: VocabularyState
pattern VocabularyState_DELETE_IN_PROGRESS = VocabularyState' "DELETE_IN_PROGRESS"

{-# COMPLETE
  VocabularyState_ACTIVE,
  VocabularyState_CREATION_FAILED,
  VocabularyState_CREATION_IN_PROGRESS,
  VocabularyState_DELETE_IN_PROGRESS,
  VocabularyState'
  #-}
