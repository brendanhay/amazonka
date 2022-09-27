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
-- Module      : Amazonka.ResilienceHub.Types.RenderRecommendationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.RenderRecommendationType
  ( RenderRecommendationType
      ( ..,
        RenderRecommendationType_Alarm,
        RenderRecommendationType_Sop,
        RenderRecommendationType_Test
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RenderRecommendationType = RenderRecommendationType'
  { fromRenderRecommendationType ::
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

pattern RenderRecommendationType_Alarm :: RenderRecommendationType
pattern RenderRecommendationType_Alarm = RenderRecommendationType' "Alarm"

pattern RenderRecommendationType_Sop :: RenderRecommendationType
pattern RenderRecommendationType_Sop = RenderRecommendationType' "Sop"

pattern RenderRecommendationType_Test :: RenderRecommendationType
pattern RenderRecommendationType_Test = RenderRecommendationType' "Test"

{-# COMPLETE
  RenderRecommendationType_Alarm,
  RenderRecommendationType_Sop,
  RenderRecommendationType_Test,
  RenderRecommendationType'
  #-}
