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
-- Module      : Network.AWS.SavingsPlans.Types.SavingsPlanRateServiceCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SavingsPlans.Types.SavingsPlanRateServiceCode
  ( SavingsPlanRateServiceCode
      ( ..,
        SavingsPlanRateServiceCode_AWSLambda,
        SavingsPlanRateServiceCode_AmazonEC2,
        SavingsPlanRateServiceCode_AmazonECS,
        SavingsPlanRateServiceCode_AmazonEKS,
        SavingsPlanRateServiceCode_AmazonSageMaker
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SavingsPlanRateServiceCode = SavingsPlanRateServiceCode'
  { fromSavingsPlanRateServiceCode ::
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

pattern SavingsPlanRateServiceCode_AWSLambda :: SavingsPlanRateServiceCode
pattern SavingsPlanRateServiceCode_AWSLambda = SavingsPlanRateServiceCode' "AWSLambda"

pattern SavingsPlanRateServiceCode_AmazonEC2 :: SavingsPlanRateServiceCode
pattern SavingsPlanRateServiceCode_AmazonEC2 = SavingsPlanRateServiceCode' "AmazonEC2"

pattern SavingsPlanRateServiceCode_AmazonECS :: SavingsPlanRateServiceCode
pattern SavingsPlanRateServiceCode_AmazonECS = SavingsPlanRateServiceCode' "AmazonECS"

pattern SavingsPlanRateServiceCode_AmazonEKS :: SavingsPlanRateServiceCode
pattern SavingsPlanRateServiceCode_AmazonEKS = SavingsPlanRateServiceCode' "AmazonEKS"

pattern SavingsPlanRateServiceCode_AmazonSageMaker :: SavingsPlanRateServiceCode
pattern SavingsPlanRateServiceCode_AmazonSageMaker = SavingsPlanRateServiceCode' "AmazonSageMaker"

{-# COMPLETE
  SavingsPlanRateServiceCode_AWSLambda,
  SavingsPlanRateServiceCode_AmazonEC2,
  SavingsPlanRateServiceCode_AmazonECS,
  SavingsPlanRateServiceCode_AmazonEKS,
  SavingsPlanRateServiceCode_AmazonSageMaker,
  SavingsPlanRateServiceCode'
  #-}
