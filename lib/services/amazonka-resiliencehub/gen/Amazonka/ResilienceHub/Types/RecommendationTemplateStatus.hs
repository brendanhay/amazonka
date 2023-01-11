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
-- Module      : Amazonka.ResilienceHub.Types.RecommendationTemplateStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.RecommendationTemplateStatus
  ( RecommendationTemplateStatus
      ( ..,
        RecommendationTemplateStatus_Failed,
        RecommendationTemplateStatus_InProgress,
        RecommendationTemplateStatus_Pending,
        RecommendationTemplateStatus_Success
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecommendationTemplateStatus = RecommendationTemplateStatus'
  { fromRecommendationTemplateStatus ::
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

pattern RecommendationTemplateStatus_Failed :: RecommendationTemplateStatus
pattern RecommendationTemplateStatus_Failed = RecommendationTemplateStatus' "Failed"

pattern RecommendationTemplateStatus_InProgress :: RecommendationTemplateStatus
pattern RecommendationTemplateStatus_InProgress = RecommendationTemplateStatus' "InProgress"

pattern RecommendationTemplateStatus_Pending :: RecommendationTemplateStatus
pattern RecommendationTemplateStatus_Pending = RecommendationTemplateStatus' "Pending"

pattern RecommendationTemplateStatus_Success :: RecommendationTemplateStatus
pattern RecommendationTemplateStatus_Success = RecommendationTemplateStatus' "Success"

{-# COMPLETE
  RecommendationTemplateStatus_Failed,
  RecommendationTemplateStatus_InProgress,
  RecommendationTemplateStatus_Pending,
  RecommendationTemplateStatus_Success,
  RecommendationTemplateStatus'
  #-}
