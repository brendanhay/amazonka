{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobAbortCriteriaFailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobAbortCriteriaFailureType
  ( AWSJobAbortCriteriaFailureType
      ( AWSJobAbortCriteriaFailureType',
        AJACFTFailed,
        AJACFTRejected,
        AJACFTTimedOut,
        AJACFTAll
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AWSJobAbortCriteriaFailureType = AWSJobAbortCriteriaFailureType' Lude.Text
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

pattern AJACFTFailed :: AWSJobAbortCriteriaFailureType
pattern AJACFTFailed = AWSJobAbortCriteriaFailureType' "FAILED"

pattern AJACFTRejected :: AWSJobAbortCriteriaFailureType
pattern AJACFTRejected = AWSJobAbortCriteriaFailureType' "REJECTED"

pattern AJACFTTimedOut :: AWSJobAbortCriteriaFailureType
pattern AJACFTTimedOut = AWSJobAbortCriteriaFailureType' "TIMED_OUT"

pattern AJACFTAll :: AWSJobAbortCriteriaFailureType
pattern AJACFTAll = AWSJobAbortCriteriaFailureType' "ALL"

{-# COMPLETE
  AJACFTFailed,
  AJACFTRejected,
  AJACFTTimedOut,
  AJACFTAll,
  AWSJobAbortCriteriaFailureType'
  #-}
