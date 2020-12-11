-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.ModelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.ModelStatus
  ( ModelStatus
      ( ModelStatus',
        MSDeleting,
        MSInError,
        MSStopRequested,
        MSStopped,
        MSSubmitted,
        MSTrained,
        MSTraining
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ModelStatus = ModelStatus' Lude.Text
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

pattern MSDeleting :: ModelStatus
pattern MSDeleting = ModelStatus' "DELETING"

pattern MSInError :: ModelStatus
pattern MSInError = ModelStatus' "IN_ERROR"

pattern MSStopRequested :: ModelStatus
pattern MSStopRequested = ModelStatus' "STOP_REQUESTED"

pattern MSStopped :: ModelStatus
pattern MSStopped = ModelStatus' "STOPPED"

pattern MSSubmitted :: ModelStatus
pattern MSSubmitted = ModelStatus' "SUBMITTED"

pattern MSTrained :: ModelStatus
pattern MSTrained = ModelStatus' "TRAINED"

pattern MSTraining :: ModelStatus
pattern MSTraining = ModelStatus' "TRAINING"

{-# COMPLETE
  MSDeleting,
  MSInError,
  MSStopRequested,
  MSStopped,
  MSSubmitted,
  MSTrained,
  MSTraining,
  ModelStatus'
  #-}
