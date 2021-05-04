{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.TaggableResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.TaggableResourceType
  ( TaggableResourceType
      ( ..,
        TaggableResourceType_BatchPrediction,
        TaggableResourceType_DataSource,
        TaggableResourceType_Evaluation,
        TaggableResourceType_MLModel
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TaggableResourceType = TaggableResourceType'
  { fromTaggableResourceType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern TaggableResourceType_BatchPrediction :: TaggableResourceType
pattern TaggableResourceType_BatchPrediction = TaggableResourceType' "BatchPrediction"

pattern TaggableResourceType_DataSource :: TaggableResourceType
pattern TaggableResourceType_DataSource = TaggableResourceType' "DataSource"

pattern TaggableResourceType_Evaluation :: TaggableResourceType
pattern TaggableResourceType_Evaluation = TaggableResourceType' "Evaluation"

pattern TaggableResourceType_MLModel :: TaggableResourceType
pattern TaggableResourceType_MLModel = TaggableResourceType' "MLModel"

{-# COMPLETE
  TaggableResourceType_BatchPrediction,
  TaggableResourceType_DataSource,
  TaggableResourceType_Evaluation,
  TaggableResourceType_MLModel,
  TaggableResourceType'
  #-}
