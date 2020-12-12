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
        BatchPrediction,
        DataSource,
        Evaluation,
        MLModel
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TaggableResourceType = TaggableResourceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern BatchPrediction :: TaggableResourceType
pattern BatchPrediction = TaggableResourceType' "BatchPrediction"

pattern DataSource :: TaggableResourceType
pattern DataSource = TaggableResourceType' "DataSource"

pattern Evaluation :: TaggableResourceType
pattern Evaluation = TaggableResourceType' "Evaluation"

pattern MLModel :: TaggableResourceType
pattern MLModel = TaggableResourceType' "MLModel"

{-# COMPLETE
  BatchPrediction,
  DataSource,
  Evaluation,
  MLModel,
  TaggableResourceType'
  #-}
