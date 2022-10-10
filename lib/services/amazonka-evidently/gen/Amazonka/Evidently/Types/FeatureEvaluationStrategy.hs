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
-- Module      : Amazonka.Evidently.Types.FeatureEvaluationStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.FeatureEvaluationStrategy
  ( FeatureEvaluationStrategy
      ( ..,
        FeatureEvaluationStrategy_ALL_RULES,
        FeatureEvaluationStrategy_DEFAULT_VARIATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FeatureEvaluationStrategy = FeatureEvaluationStrategy'
  { fromFeatureEvaluationStrategy ::
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

pattern FeatureEvaluationStrategy_ALL_RULES :: FeatureEvaluationStrategy
pattern FeatureEvaluationStrategy_ALL_RULES = FeatureEvaluationStrategy' "ALL_RULES"

pattern FeatureEvaluationStrategy_DEFAULT_VARIATION :: FeatureEvaluationStrategy
pattern FeatureEvaluationStrategy_DEFAULT_VARIATION = FeatureEvaluationStrategy' "DEFAULT_VARIATION"

{-# COMPLETE
  FeatureEvaluationStrategy_ALL_RULES,
  FeatureEvaluationStrategy_DEFAULT_VARIATION,
  FeatureEvaluationStrategy'
  #-}
