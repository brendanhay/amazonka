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
-- Module      : Amazonka.SageMaker.Types.AutoMLAlgorithm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLAlgorithm
  ( AutoMLAlgorithm
      ( ..,
        AutoMLAlgorithm_Catboost,
        AutoMLAlgorithm_Extra_trees,
        AutoMLAlgorithm_Fastai,
        AutoMLAlgorithm_Lightgbm,
        AutoMLAlgorithm_Linear_learner,
        AutoMLAlgorithm_Mlp,
        AutoMLAlgorithm_Nn_torch,
        AutoMLAlgorithm_Randomforest,
        AutoMLAlgorithm_Xgboost
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoMLAlgorithm = AutoMLAlgorithm'
  { fromAutoMLAlgorithm ::
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

pattern AutoMLAlgorithm_Catboost :: AutoMLAlgorithm
pattern AutoMLAlgorithm_Catboost = AutoMLAlgorithm' "catboost"

pattern AutoMLAlgorithm_Extra_trees :: AutoMLAlgorithm
pattern AutoMLAlgorithm_Extra_trees = AutoMLAlgorithm' "extra-trees"

pattern AutoMLAlgorithm_Fastai :: AutoMLAlgorithm
pattern AutoMLAlgorithm_Fastai = AutoMLAlgorithm' "fastai"

pattern AutoMLAlgorithm_Lightgbm :: AutoMLAlgorithm
pattern AutoMLAlgorithm_Lightgbm = AutoMLAlgorithm' "lightgbm"

pattern AutoMLAlgorithm_Linear_learner :: AutoMLAlgorithm
pattern AutoMLAlgorithm_Linear_learner = AutoMLAlgorithm' "linear-learner"

pattern AutoMLAlgorithm_Mlp :: AutoMLAlgorithm
pattern AutoMLAlgorithm_Mlp = AutoMLAlgorithm' "mlp"

pattern AutoMLAlgorithm_Nn_torch :: AutoMLAlgorithm
pattern AutoMLAlgorithm_Nn_torch = AutoMLAlgorithm' "nn-torch"

pattern AutoMLAlgorithm_Randomforest :: AutoMLAlgorithm
pattern AutoMLAlgorithm_Randomforest = AutoMLAlgorithm' "randomforest"

pattern AutoMLAlgorithm_Xgboost :: AutoMLAlgorithm
pattern AutoMLAlgorithm_Xgboost = AutoMLAlgorithm' "xgboost"

{-# COMPLETE
  AutoMLAlgorithm_Catboost,
  AutoMLAlgorithm_Extra_trees,
  AutoMLAlgorithm_Fastai,
  AutoMLAlgorithm_Lightgbm,
  AutoMLAlgorithm_Linear_learner,
  AutoMLAlgorithm_Mlp,
  AutoMLAlgorithm_Nn_torch,
  AutoMLAlgorithm_Randomforest,
  AutoMLAlgorithm_Xgboost,
  AutoMLAlgorithm'
  #-}
