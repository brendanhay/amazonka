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
-- Module      : Amazonka.ResilienceHub.Types.AssessmentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.AssessmentStatus
  ( AssessmentStatus
      ( ..,
        AssessmentStatus_Failed,
        AssessmentStatus_InProgress,
        AssessmentStatus_Pending,
        AssessmentStatus_Success
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AssessmentStatus = AssessmentStatus'
  { fromAssessmentStatus ::
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

pattern AssessmentStatus_Failed :: AssessmentStatus
pattern AssessmentStatus_Failed = AssessmentStatus' "Failed"

pattern AssessmentStatus_InProgress :: AssessmentStatus
pattern AssessmentStatus_InProgress = AssessmentStatus' "InProgress"

pattern AssessmentStatus_Pending :: AssessmentStatus
pattern AssessmentStatus_Pending = AssessmentStatus' "Pending"

pattern AssessmentStatus_Success :: AssessmentStatus
pattern AssessmentStatus_Success = AssessmentStatus' "Success"

{-# COMPLETE
  AssessmentStatus_Failed,
  AssessmentStatus_InProgress,
  AssessmentStatus_Pending,
  AssessmentStatus_Success,
  AssessmentStatus'
  #-}
