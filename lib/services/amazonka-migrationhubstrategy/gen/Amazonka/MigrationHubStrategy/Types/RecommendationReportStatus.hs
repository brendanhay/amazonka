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
-- Module      : Amazonka.MigrationHubStrategy.Types.RecommendationReportStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.RecommendationReportStatus
  ( RecommendationReportStatus
      ( ..,
        RecommendationReportStatus_FAILED,
        RecommendationReportStatus_IN_PROGRESS,
        RecommendationReportStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecommendationReportStatus = RecommendationReportStatus'
  { fromRecommendationReportStatus ::
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

pattern RecommendationReportStatus_FAILED :: RecommendationReportStatus
pattern RecommendationReportStatus_FAILED = RecommendationReportStatus' "FAILED"

pattern RecommendationReportStatus_IN_PROGRESS :: RecommendationReportStatus
pattern RecommendationReportStatus_IN_PROGRESS = RecommendationReportStatus' "IN_PROGRESS"

pattern RecommendationReportStatus_SUCCESS :: RecommendationReportStatus
pattern RecommendationReportStatus_SUCCESS = RecommendationReportStatus' "SUCCESS"

{-# COMPLETE
  RecommendationReportStatus_FAILED,
  RecommendationReportStatus_IN_PROGRESS,
  RecommendationReportStatus_SUCCESS,
  RecommendationReportStatus'
  #-}
