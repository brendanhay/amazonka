{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AwsJobAbortCriteriaFailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AwsJobAbortCriteriaFailureType
  ( AwsJobAbortCriteriaFailureType
    ( AwsJobAbortCriteriaFailureType'
    , AwsJobAbortCriteriaFailureTypeFailed
    , AwsJobAbortCriteriaFailureTypeRejected
    , AwsJobAbortCriteriaFailureTypeTimedOut
    , AwsJobAbortCriteriaFailureTypeAll
    , fromAwsJobAbortCriteriaFailureType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AwsJobAbortCriteriaFailureType = AwsJobAbortCriteriaFailureType'{fromAwsJobAbortCriteriaFailureType
                                                                         :: Core.Text}
                                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                           Core.Generic)
                                           deriving newtype (Core.IsString, Core.Hashable,
                                                             Core.NFData, Core.ToJSONKey,
                                                             Core.FromJSONKey, Core.ToJSON,
                                                             Core.FromJSON, Core.ToXML,
                                                             Core.FromXML, Core.ToText,
                                                             Core.FromText, Core.ToByteString,
                                                             Core.ToQuery, Core.ToHeader)

pattern AwsJobAbortCriteriaFailureTypeFailed :: AwsJobAbortCriteriaFailureType
pattern AwsJobAbortCriteriaFailureTypeFailed = AwsJobAbortCriteriaFailureType' "FAILED"

pattern AwsJobAbortCriteriaFailureTypeRejected :: AwsJobAbortCriteriaFailureType
pattern AwsJobAbortCriteriaFailureTypeRejected = AwsJobAbortCriteriaFailureType' "REJECTED"

pattern AwsJobAbortCriteriaFailureTypeTimedOut :: AwsJobAbortCriteriaFailureType
pattern AwsJobAbortCriteriaFailureTypeTimedOut = AwsJobAbortCriteriaFailureType' "TIMED_OUT"

pattern AwsJobAbortCriteriaFailureTypeAll :: AwsJobAbortCriteriaFailureType
pattern AwsJobAbortCriteriaFailureTypeAll = AwsJobAbortCriteriaFailureType' "ALL"

{-# COMPLETE 
  AwsJobAbortCriteriaFailureTypeFailed,

  AwsJobAbortCriteriaFailureTypeRejected,

  AwsJobAbortCriteriaFailureTypeTimedOut,

  AwsJobAbortCriteriaFailureTypeAll,
  AwsJobAbortCriteriaFailureType'
  #-}
