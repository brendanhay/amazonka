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
-- Module      : Network.AWS.Transcribe.Types.VocabularyState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyState
  ( VocabularyState
      ( ..,
        VocabularyState_FAILED,
        VocabularyState_PENDING,
        VocabularyState_READY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype VocabularyState = VocabularyState'
  { fromVocabularyState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern VocabularyState_FAILED :: VocabularyState
pattern VocabularyState_FAILED = VocabularyState' "FAILED"

pattern VocabularyState_PENDING :: VocabularyState
pattern VocabularyState_PENDING = VocabularyState' "PENDING"

pattern VocabularyState_READY :: VocabularyState
pattern VocabularyState_READY = VocabularyState' "READY"

{-# COMPLETE
  VocabularyState_FAILED,
  VocabularyState_PENDING,
  VocabularyState_READY,
  VocabularyState'
  #-}
