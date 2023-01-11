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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFilterName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFilterName
  ( ECSServiceRecommendationFilterName
      ( ..,
        ECSServiceRecommendationFilterName_Finding,
        ECSServiceRecommendationFilterName_FindingReasonCode
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ECSServiceRecommendationFilterName = ECSServiceRecommendationFilterName'
  { fromECSServiceRecommendationFilterName ::
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

pattern ECSServiceRecommendationFilterName_Finding :: ECSServiceRecommendationFilterName
pattern ECSServiceRecommendationFilterName_Finding = ECSServiceRecommendationFilterName' "Finding"

pattern ECSServiceRecommendationFilterName_FindingReasonCode :: ECSServiceRecommendationFilterName
pattern ECSServiceRecommendationFilterName_FindingReasonCode = ECSServiceRecommendationFilterName' "FindingReasonCode"

{-# COMPLETE
  ECSServiceRecommendationFilterName_Finding,
  ECSServiceRecommendationFilterName_FindingReasonCode,
  ECSServiceRecommendationFilterName'
  #-}
