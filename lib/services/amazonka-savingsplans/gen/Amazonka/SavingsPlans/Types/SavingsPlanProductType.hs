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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanProductType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanProductType
  ( SavingsPlanProductType
      ( ..,
        SavingsPlanProductType_EC2,
        SavingsPlanProductType_Fargate,
        SavingsPlanProductType_Lambda,
        SavingsPlanProductType_SageMaker
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SavingsPlanProductType = SavingsPlanProductType'
  { fromSavingsPlanProductType ::
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

pattern SavingsPlanProductType_EC2 :: SavingsPlanProductType
pattern SavingsPlanProductType_EC2 = SavingsPlanProductType' "EC2"

pattern SavingsPlanProductType_Fargate :: SavingsPlanProductType
pattern SavingsPlanProductType_Fargate = SavingsPlanProductType' "Fargate"

pattern SavingsPlanProductType_Lambda :: SavingsPlanProductType
pattern SavingsPlanProductType_Lambda = SavingsPlanProductType' "Lambda"

pattern SavingsPlanProductType_SageMaker :: SavingsPlanProductType
pattern SavingsPlanProductType_SageMaker = SavingsPlanProductType' "SageMaker"

{-# COMPLETE
  SavingsPlanProductType_EC2,
  SavingsPlanProductType_Fargate,
  SavingsPlanProductType_Lambda,
  SavingsPlanProductType_SageMaker,
  SavingsPlanProductType'
  #-}
