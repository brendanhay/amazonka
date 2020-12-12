{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.VocabularyState
  ( VocabularyState
      ( VocabularyState',
        Failed,
        Pending,
        Ready
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VocabularyState = VocabularyState' Lude.Text
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

pattern Failed :: VocabularyState
pattern Failed = VocabularyState' "FAILED"

pattern Pending :: VocabularyState
pattern Pending = VocabularyState' "PENDING"

pattern Ready :: VocabularyState
pattern Ready = VocabularyState' "READY"

{-# COMPLETE
  Failed,
  Pending,
  Ready,
  VocabularyState'
  #-}
