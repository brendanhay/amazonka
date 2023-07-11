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
-- Module      : Amazonka.ComputeOptimizer.Types.RecommendationSourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.RecommendationSourceType
  ( RecommendationSourceType
      ( ..,
        RecommendationSourceType_AutoScalingGroup,
        RecommendationSourceType_EbsVolume,
        RecommendationSourceType_Ec2Instance,
        RecommendationSourceType_EcsService,
        RecommendationSourceType_LambdaFunction
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecommendationSourceType = RecommendationSourceType'
  { fromRecommendationSourceType ::
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

pattern RecommendationSourceType_AutoScalingGroup :: RecommendationSourceType
pattern RecommendationSourceType_AutoScalingGroup = RecommendationSourceType' "AutoScalingGroup"

pattern RecommendationSourceType_EbsVolume :: RecommendationSourceType
pattern RecommendationSourceType_EbsVolume = RecommendationSourceType' "EbsVolume"

pattern RecommendationSourceType_Ec2Instance :: RecommendationSourceType
pattern RecommendationSourceType_Ec2Instance = RecommendationSourceType' "Ec2Instance"

pattern RecommendationSourceType_EcsService :: RecommendationSourceType
pattern RecommendationSourceType_EcsService = RecommendationSourceType' "EcsService"

pattern RecommendationSourceType_LambdaFunction :: RecommendationSourceType
pattern RecommendationSourceType_LambdaFunction = RecommendationSourceType' "LambdaFunction"

{-# COMPLETE
  RecommendationSourceType_AutoScalingGroup,
  RecommendationSourceType_EbsVolume,
  RecommendationSourceType_Ec2Instance,
  RecommendationSourceType_EcsService,
  RecommendationSourceType_LambdaFunction,
  RecommendationSourceType'
  #-}
