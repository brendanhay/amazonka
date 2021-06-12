{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AwsJobAbortCriteriaFailureType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AwsJobAbortCriteriaFailureType
  ( AwsJobAbortCriteriaFailureType
      ( ..,
        AwsJobAbortCriteriaFailureType_ALL,
        AwsJobAbortCriteriaFailureType_FAILED,
        AwsJobAbortCriteriaFailureType_REJECTED,
        AwsJobAbortCriteriaFailureType_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AwsJobAbortCriteriaFailureType = AwsJobAbortCriteriaFailureType'
  { fromAwsJobAbortCriteriaFailureType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AwsJobAbortCriteriaFailureType_ALL :: AwsJobAbortCriteriaFailureType
pattern AwsJobAbortCriteriaFailureType_ALL = AwsJobAbortCriteriaFailureType' "ALL"

pattern AwsJobAbortCriteriaFailureType_FAILED :: AwsJobAbortCriteriaFailureType
pattern AwsJobAbortCriteriaFailureType_FAILED = AwsJobAbortCriteriaFailureType' "FAILED"

pattern AwsJobAbortCriteriaFailureType_REJECTED :: AwsJobAbortCriteriaFailureType
pattern AwsJobAbortCriteriaFailureType_REJECTED = AwsJobAbortCriteriaFailureType' "REJECTED"

pattern AwsJobAbortCriteriaFailureType_TIMED_OUT :: AwsJobAbortCriteriaFailureType
pattern AwsJobAbortCriteriaFailureType_TIMED_OUT = AwsJobAbortCriteriaFailureType' "TIMED_OUT"

{-# COMPLETE
  AwsJobAbortCriteriaFailureType_ALL,
  AwsJobAbortCriteriaFailureType_FAILED,
  AwsJobAbortCriteriaFailureType_REJECTED,
  AwsJobAbortCriteriaFailureType_TIMED_OUT,
  AwsJobAbortCriteriaFailureType'
  #-}
