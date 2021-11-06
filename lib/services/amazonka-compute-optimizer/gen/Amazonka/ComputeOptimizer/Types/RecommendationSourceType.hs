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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.RecommendationSourceType
  ( RecommendationSourceType
      ( ..,
        RecommendationSourceType_AutoScalingGroup,
        RecommendationSourceType_EbsVolume,
        RecommendationSourceType_Ec2Instance,
        RecommendationSourceType_LambdaFunction
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RecommendationSourceType = RecommendationSourceType'
  { fromRecommendationSourceType ::
      Core.Text
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

pattern RecommendationSourceType_AutoScalingGroup :: RecommendationSourceType
pattern RecommendationSourceType_AutoScalingGroup = RecommendationSourceType' "AutoScalingGroup"

pattern RecommendationSourceType_EbsVolume :: RecommendationSourceType
pattern RecommendationSourceType_EbsVolume = RecommendationSourceType' "EbsVolume"

pattern RecommendationSourceType_Ec2Instance :: RecommendationSourceType
pattern RecommendationSourceType_Ec2Instance = RecommendationSourceType' "Ec2Instance"

pattern RecommendationSourceType_LambdaFunction :: RecommendationSourceType
pattern RecommendationSourceType_LambdaFunction = RecommendationSourceType' "LambdaFunction"

{-# COMPLETE
  RecommendationSourceType_AutoScalingGroup,
  RecommendationSourceType_EbsVolume,
  RecommendationSourceType_Ec2Instance,
  RecommendationSourceType_LambdaFunction,
  RecommendationSourceType'
  #-}
