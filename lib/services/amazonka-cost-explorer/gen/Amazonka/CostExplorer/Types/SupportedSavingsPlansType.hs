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
-- Module      : Amazonka.CostExplorer.Types.SupportedSavingsPlansType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SupportedSavingsPlansType
  ( SupportedSavingsPlansType
      ( ..,
        SupportedSavingsPlansType_COMPUTE_SP,
        SupportedSavingsPlansType_EC2_INSTANCE_SP,
        SupportedSavingsPlansType_SAGEMAKER_SP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SupportedSavingsPlansType = SupportedSavingsPlansType'
  { fromSupportedSavingsPlansType ::
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

pattern SupportedSavingsPlansType_COMPUTE_SP :: SupportedSavingsPlansType
pattern SupportedSavingsPlansType_COMPUTE_SP = SupportedSavingsPlansType' "COMPUTE_SP"

pattern SupportedSavingsPlansType_EC2_INSTANCE_SP :: SupportedSavingsPlansType
pattern SupportedSavingsPlansType_EC2_INSTANCE_SP = SupportedSavingsPlansType' "EC2_INSTANCE_SP"

pattern SupportedSavingsPlansType_SAGEMAKER_SP :: SupportedSavingsPlansType
pattern SupportedSavingsPlansType_SAGEMAKER_SP = SupportedSavingsPlansType' "SAGEMAKER_SP"

{-# COMPLETE
  SupportedSavingsPlansType_COMPUTE_SP,
  SupportedSavingsPlansType_EC2_INSTANCE_SP,
  SupportedSavingsPlansType_SAGEMAKER_SP,
  SupportedSavingsPlansType'
  #-}
