{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionFailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionFailureType
  ( JobExecutionFailureType
      ( JobExecutionFailureType',
        JEFTFailed,
        JEFTRejected,
        JEFTTimedOut,
        JEFTAll
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype JobExecutionFailureType = JobExecutionFailureType' Lude.Text
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

pattern JEFTFailed :: JobExecutionFailureType
pattern JEFTFailed = JobExecutionFailureType' "FAILED"

pattern JEFTRejected :: JobExecutionFailureType
pattern JEFTRejected = JobExecutionFailureType' "REJECTED"

pattern JEFTTimedOut :: JobExecutionFailureType
pattern JEFTTimedOut = JobExecutionFailureType' "TIMED_OUT"

pattern JEFTAll :: JobExecutionFailureType
pattern JEFTAll = JobExecutionFailureType' "ALL"

{-# COMPLETE
  JEFTFailed,
  JEFTRejected,
  JEFTTimedOut,
  JEFTAll,
  JobExecutionFailureType'
  #-}
