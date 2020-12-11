-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobStatus
  ( AutoMLJobStatus
      ( AutoMLJobStatus',
        AMLJSCompleted,
        AMLJSFailed,
        AMLJSInProgress,
        AMLJSStopped,
        AMLJSStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoMLJobStatus = AutoMLJobStatus' Lude.Text
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

pattern AMLJSCompleted :: AutoMLJobStatus
pattern AMLJSCompleted = AutoMLJobStatus' "Completed"

pattern AMLJSFailed :: AutoMLJobStatus
pattern AMLJSFailed = AutoMLJobStatus' "Failed"

pattern AMLJSInProgress :: AutoMLJobStatus
pattern AMLJSInProgress = AutoMLJobStatus' "InProgress"

pattern AMLJSStopped :: AutoMLJobStatus
pattern AMLJSStopped = AutoMLJobStatus' "Stopped"

pattern AMLJSStopping :: AutoMLJobStatus
pattern AMLJSStopping = AutoMLJobStatus' "Stopping"

{-# COMPLETE
  AMLJSCompleted,
  AMLJSFailed,
  AMLJSInProgress,
  AMLJSStopped,
  AMLJSStopping,
  AutoMLJobStatus'
  #-}
