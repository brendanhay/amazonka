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
-- Module      : Amazonka.MachineLearning.Types.TaggableResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.TaggableResourceType
  ( TaggableResourceType
      ( ..,
        TaggableResourceType_BatchPrediction,
        TaggableResourceType_DataSource,
        TaggableResourceType_Evaluation,
        TaggableResourceType_MLModel
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TaggableResourceType = TaggableResourceType'
  { fromTaggableResourceType ::
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
