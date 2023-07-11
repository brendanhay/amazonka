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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssessmentStatus = AssessmentStatus'
  { fromAssessmentStatus ::
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
