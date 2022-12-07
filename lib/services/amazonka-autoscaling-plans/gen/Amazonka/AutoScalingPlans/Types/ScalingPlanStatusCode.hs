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
-- Module      : Amazonka.AutoScalingPlans.Types.ScalingPlanStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.ScalingPlanStatusCode
  ( ScalingPlanStatusCode
      ( ..,
        ScalingPlanStatusCode_Active,
        ScalingPlanStatusCode_ActiveWithProblems,
        ScalingPlanStatusCode_CreationFailed,
        ScalingPlanStatusCode_CreationInProgress,
        ScalingPlanStatusCode_DeletionFailed,
        ScalingPlanStatusCode_DeletionInProgress,
        ScalingPlanStatusCode_UpdateFailed,
        ScalingPlanStatusCode_UpdateInProgress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScalingPlanStatusCode = ScalingPlanStatusCode'
  { fromScalingPlanStatusCode ::
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

pattern ScalingPlanStatusCode_Active :: ScalingPlanStatusCode
pattern ScalingPlanStatusCode_Active = ScalingPlanStatusCode' "Active"

pattern ScalingPlanStatusCode_ActiveWithProblems :: ScalingPlanStatusCode
pattern ScalingPlanStatusCode_ActiveWithProblems = ScalingPlanStatusCode' "ActiveWithProblems"

pattern ScalingPlanStatusCode_CreationFailed :: ScalingPlanStatusCode
pattern ScalingPlanStatusCode_CreationFailed = ScalingPlanStatusCode' "CreationFailed"

pattern ScalingPlanStatusCode_CreationInProgress :: ScalingPlanStatusCode
pattern ScalingPlanStatusCode_CreationInProgress = ScalingPlanStatusCode' "CreationInProgress"

pattern ScalingPlanStatusCode_DeletionFailed :: ScalingPlanStatusCode
pattern ScalingPlanStatusCode_DeletionFailed = ScalingPlanStatusCode' "DeletionFailed"

pattern ScalingPlanStatusCode_DeletionInProgress :: ScalingPlanStatusCode
pattern ScalingPlanStatusCode_DeletionInProgress = ScalingPlanStatusCode' "DeletionInProgress"

pattern ScalingPlanStatusCode_UpdateFailed :: ScalingPlanStatusCode
pattern ScalingPlanStatusCode_UpdateFailed = ScalingPlanStatusCode' "UpdateFailed"

pattern ScalingPlanStatusCode_UpdateInProgress :: ScalingPlanStatusCode
pattern ScalingPlanStatusCode_UpdateInProgress = ScalingPlanStatusCode' "UpdateInProgress"

{-# COMPLETE
  ScalingPlanStatusCode_Active,
  ScalingPlanStatusCode_ActiveWithProblems,
  ScalingPlanStatusCode_CreationFailed,
  ScalingPlanStatusCode_CreationInProgress,
  ScalingPlanStatusCode_DeletionFailed,
  ScalingPlanStatusCode_DeletionInProgress,
  ScalingPlanStatusCode_UpdateFailed,
  ScalingPlanStatusCode_UpdateInProgress,
  ScalingPlanStatusCode'
  #-}
