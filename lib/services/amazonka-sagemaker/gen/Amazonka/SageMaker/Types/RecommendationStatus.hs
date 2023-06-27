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
-- Module      : Amazonka.SageMaker.Types.RecommendationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationStatus
  ( RecommendationStatus
      ( ..,
        RecommendationStatus_COMPLETED,
        RecommendationStatus_FAILED,
        RecommendationStatus_IN_PROGRESS,
        RecommendationStatus_NOT_APPLICABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecommendationStatus = RecommendationStatus'
  { fromRecommendationStatus ::
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

pattern RecommendationStatus_COMPLETED :: RecommendationStatus
pattern RecommendationStatus_COMPLETED = RecommendationStatus' "COMPLETED"

pattern RecommendationStatus_FAILED :: RecommendationStatus
pattern RecommendationStatus_FAILED = RecommendationStatus' "FAILED"

pattern RecommendationStatus_IN_PROGRESS :: RecommendationStatus
pattern RecommendationStatus_IN_PROGRESS = RecommendationStatus' "IN_PROGRESS"

pattern RecommendationStatus_NOT_APPLICABLE :: RecommendationStatus
pattern RecommendationStatus_NOT_APPLICABLE = RecommendationStatus' "NOT_APPLICABLE"

{-# COMPLETE
  RecommendationStatus_COMPLETED,
  RecommendationStatus_FAILED,
  RecommendationStatus_IN_PROGRESS,
  RecommendationStatus_NOT_APPLICABLE,
  RecommendationStatus'
  #-}
