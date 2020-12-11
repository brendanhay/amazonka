-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmStatus
  ( AlgorithmStatus
      ( AlgorithmStatus',
        ACompleted,
        ADeleting,
        AFailed,
        AInProgress,
        APending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AlgorithmStatus = AlgorithmStatus' Lude.Text
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

pattern ACompleted :: AlgorithmStatus
pattern ACompleted = AlgorithmStatus' "Completed"

pattern ADeleting :: AlgorithmStatus
pattern ADeleting = AlgorithmStatus' "Deleting"

pattern AFailed :: AlgorithmStatus
pattern AFailed = AlgorithmStatus' "Failed"

pattern AInProgress :: AlgorithmStatus
pattern AInProgress = AlgorithmStatus' "InProgress"

pattern APending :: AlgorithmStatus
pattern APending = AlgorithmStatus' "Pending"

{-# COMPLETE
  ACompleted,
  ADeleting,
  AFailed,
  AInProgress,
  APending,
  AlgorithmStatus'
  #-}
