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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype RecommendationTemplateStatus = RecommendationTemplateStatus'
  { fromRecommendationTemplateStatus ::
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
