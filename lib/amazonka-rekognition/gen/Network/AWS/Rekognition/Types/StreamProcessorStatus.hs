{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorStatus
  ( StreamProcessorStatus
      ( StreamProcessorStatus',
        SPSStopped,
        SPSStarting,
        SPSRunning,
        SPSFailed,
        SPSStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StreamProcessorStatus = StreamProcessorStatus' Lude.Text
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

pattern SPSStopped :: StreamProcessorStatus
pattern SPSStopped = StreamProcessorStatus' "STOPPED"

pattern SPSStarting :: StreamProcessorStatus
pattern SPSStarting = StreamProcessorStatus' "STARTING"

pattern SPSRunning :: StreamProcessorStatus
pattern SPSRunning = StreamProcessorStatus' "RUNNING"

pattern SPSFailed :: StreamProcessorStatus
pattern SPSFailed = StreamProcessorStatus' "FAILED"

pattern SPSStopping :: StreamProcessorStatus
pattern SPSStopping = StreamProcessorStatus' "STOPPING"

{-# COMPLETE
  SPSStopped,
  SPSStarting,
  SPSRunning,
  SPSFailed,
  SPSStopping,
  StreamProcessorStatus'
  #-}
