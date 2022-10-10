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
-- Module      : Amazonka.ResilienceHub.Types.AppAssessmentScheduleType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.AppAssessmentScheduleType
  ( AppAssessmentScheduleType
      ( ..,
        AppAssessmentScheduleType_Daily,
        AppAssessmentScheduleType_Disabled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AppAssessmentScheduleType = AppAssessmentScheduleType'
  { fromAppAssessmentScheduleType ::
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

pattern AppAssessmentScheduleType_Daily :: AppAssessmentScheduleType
pattern AppAssessmentScheduleType_Daily = AppAssessmentScheduleType' "Daily"

pattern AppAssessmentScheduleType_Disabled :: AppAssessmentScheduleType
pattern AppAssessmentScheduleType_Disabled = AppAssessmentScheduleType' "Disabled"

{-# COMPLETE
  AppAssessmentScheduleType_Daily,
  AppAssessmentScheduleType_Disabled,
  AppAssessmentScheduleType'
  #-}
