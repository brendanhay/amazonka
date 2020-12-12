{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobStatus
  ( TrainingJobStatus
      ( TrainingJobStatus',
        TJSCompleted,
        TJSFailed,
        TJSInProgress,
        TJSStopped,
        TJSStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TrainingJobStatus = TrainingJobStatus' Lude.Text
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

pattern TJSCompleted :: TrainingJobStatus
pattern TJSCompleted = TrainingJobStatus' "Completed"

pattern TJSFailed :: TrainingJobStatus
pattern TJSFailed = TrainingJobStatus' "Failed"

pattern TJSInProgress :: TrainingJobStatus
pattern TJSInProgress = TrainingJobStatus' "InProgress"

pattern TJSStopped :: TrainingJobStatus
pattern TJSStopped = TrainingJobStatus' "Stopped"

pattern TJSStopping :: TrainingJobStatus
pattern TJSStopping = TrainingJobStatus' "Stopping"

{-# COMPLETE
  TJSCompleted,
  TJSFailed,
  TJSInProgress,
  TJSStopped,
  TJSStopping,
  TrainingJobStatus'
  #-}
