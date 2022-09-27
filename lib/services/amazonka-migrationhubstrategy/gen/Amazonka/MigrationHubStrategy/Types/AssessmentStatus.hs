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
-- Module      : Amazonka.MigrationHubStrategy.Types.AssessmentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AssessmentStatus
  ( AssessmentStatus
      ( ..,
        AssessmentStatus_COMPLETE,
        AssessmentStatus_FAILED,
        AssessmentStatus_IN_PROGRESS,
        AssessmentStatus_STOPPED
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

pattern AssessmentStatus_COMPLETE :: AssessmentStatus
pattern AssessmentStatus_COMPLETE = AssessmentStatus' "COMPLETE"

pattern AssessmentStatus_FAILED :: AssessmentStatus
pattern AssessmentStatus_FAILED = AssessmentStatus' "FAILED"

pattern AssessmentStatus_IN_PROGRESS :: AssessmentStatus
pattern AssessmentStatus_IN_PROGRESS = AssessmentStatus' "IN_PROGRESS"

pattern AssessmentStatus_STOPPED :: AssessmentStatus
pattern AssessmentStatus_STOPPED = AssessmentStatus' "STOPPED"

{-# COMPLETE
  AssessmentStatus_COMPLETE,
  AssessmentStatus_FAILED,
  AssessmentStatus_IN_PROGRESS,
  AssessmentStatus_STOPPED,
  AssessmentStatus'
  #-}
