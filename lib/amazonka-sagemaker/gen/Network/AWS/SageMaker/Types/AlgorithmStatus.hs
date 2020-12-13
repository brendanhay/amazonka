{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        APending,
        AInProgress,
        ACompleted,
        AFailed,
        ADeleting
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

pattern APending :: AlgorithmStatus
pattern APending = AlgorithmStatus' "Pending"

pattern AInProgress :: AlgorithmStatus
pattern AInProgress = AlgorithmStatus' "InProgress"

pattern ACompleted :: AlgorithmStatus
pattern ACompleted = AlgorithmStatus' "Completed"

pattern AFailed :: AlgorithmStatus
pattern AFailed = AlgorithmStatus' "Failed"

pattern ADeleting :: AlgorithmStatus
pattern ADeleting = AlgorithmStatus' "Deleting"

{-# COMPLETE
  APending,
  AInProgress,
  ACompleted,
  AFailed,
  ADeleting,
  AlgorithmStatus'
  #-}
