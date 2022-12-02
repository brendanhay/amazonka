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
-- Module      : Amazonka.IoT.Types.AwsJobAbortCriteriaFailureType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AwsJobAbortCriteriaFailureType
  ( AwsJobAbortCriteriaFailureType
      ( ..,
        AwsJobAbortCriteriaFailureType_ALL,
        AwsJobAbortCriteriaFailureType_FAILED,
        AwsJobAbortCriteriaFailureType_REJECTED,
        AwsJobAbortCriteriaFailureType_TIMED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AwsJobAbortCriteriaFailureType = AwsJobAbortCriteriaFailureType'
  { fromAwsJobAbortCriteriaFailureType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
