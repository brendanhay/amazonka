{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.TaggableResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.TaggableResourceType
  ( TaggableResourceType
      ( TaggableResourceType',
        TaggableResourceTypeBatchPrediction,
        TaggableResourceTypeDataSource,
        TaggableResourceTypeEvaluation,
        TaggableResourceTypeMLModel,
        fromTaggableResourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TaggableResourceType = TaggableResourceType'
  { fromTaggableResourceType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern TaggableResourceTypeBatchPrediction :: TaggableResourceType
pattern TaggableResourceTypeBatchPrediction = TaggableResourceType' "BatchPrediction"

pattern TaggableResourceTypeDataSource :: TaggableResourceType
pattern TaggableResourceTypeDataSource = TaggableResourceType' "DataSource"

pattern TaggableResourceTypeEvaluation :: TaggableResourceType
pattern TaggableResourceTypeEvaluation = TaggableResourceType' "Evaluation"

pattern TaggableResourceTypeMLModel :: TaggableResourceType
pattern TaggableResourceTypeMLModel = TaggableResourceType' "MLModel"

{-# COMPLETE
  TaggableResourceTypeBatchPrediction,
  TaggableResourceTypeDataSource,
  TaggableResourceTypeEvaluation,
  TaggableResourceTypeMLModel,
  TaggableResourceType'
  #-}
