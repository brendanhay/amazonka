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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanRateServiceCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanRateServiceCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SavingsPlanRateServiceCode = SavingsPlanRateServiceCode'
  { fromSavingsPlanRateServiceCode ::
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
