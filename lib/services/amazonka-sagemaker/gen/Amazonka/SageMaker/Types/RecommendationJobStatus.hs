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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobStatus
  ( RecommendationJobStatus
      ( ..,
        RecommendationJobStatus_COMPLETED,
        RecommendationJobStatus_FAILED,
        RecommendationJobStatus_IN_PROGRESS,
        RecommendationJobStatus_PENDING,
        RecommendationJobStatus_STOPPED,
        RecommendationJobStatus_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecommendationJobStatus = RecommendationJobStatus'
  { fromRecommendationJobStatus ::
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

pattern RecommendationJobStatus_COMPLETED :: RecommendationJobStatus
pattern RecommendationJobStatus_COMPLETED = RecommendationJobStatus' "COMPLETED"

pattern RecommendationJobStatus_FAILED :: RecommendationJobStatus
pattern RecommendationJobStatus_FAILED = RecommendationJobStatus' "FAILED"

pattern RecommendationJobStatus_IN_PROGRESS :: RecommendationJobStatus
pattern RecommendationJobStatus_IN_PROGRESS = RecommendationJobStatus' "IN_PROGRESS"

pattern RecommendationJobStatus_PENDING :: RecommendationJobStatus
pattern RecommendationJobStatus_PENDING = RecommendationJobStatus' "PENDING"

pattern RecommendationJobStatus_STOPPED :: RecommendationJobStatus
pattern RecommendationJobStatus_STOPPED = RecommendationJobStatus' "STOPPED"

pattern RecommendationJobStatus_STOPPING :: RecommendationJobStatus
pattern RecommendationJobStatus_STOPPING = RecommendationJobStatus' "STOPPING"

{-# COMPLETE
  RecommendationJobStatus_COMPLETED,
  RecommendationJobStatus_FAILED,
  RecommendationJobStatus_IN_PROGRESS,
  RecommendationJobStatus_PENDING,
  RecommendationJobStatus_STOPPED,
  RecommendationJobStatus_STOPPING,
  RecommendationJobStatus'
  #-}
