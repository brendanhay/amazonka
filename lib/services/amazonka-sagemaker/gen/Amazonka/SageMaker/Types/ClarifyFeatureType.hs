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
-- Module      : Amazonka.SageMaker.Types.ClarifyFeatureType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ClarifyFeatureType
  ( ClarifyFeatureType
      ( ..,
        ClarifyFeatureType_Categorical,
        ClarifyFeatureType_Numerical,
        ClarifyFeatureType_Text
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClarifyFeatureType = ClarifyFeatureType'
  { fromClarifyFeatureType ::
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

pattern ClarifyFeatureType_Categorical :: ClarifyFeatureType
pattern ClarifyFeatureType_Categorical = ClarifyFeatureType' "categorical"

pattern ClarifyFeatureType_Numerical :: ClarifyFeatureType
pattern ClarifyFeatureType_Numerical = ClarifyFeatureType' "numerical"

pattern ClarifyFeatureType_Text :: ClarifyFeatureType
pattern ClarifyFeatureType_Text = ClarifyFeatureType' "text"

{-# COMPLETE
  ClarifyFeatureType_Categorical,
  ClarifyFeatureType_Numerical,
  ClarifyFeatureType_Text,
  ClarifyFeatureType'
  #-}
