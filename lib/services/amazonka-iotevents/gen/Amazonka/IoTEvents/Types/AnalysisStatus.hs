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
-- Module      : Amazonka.IoTEvents.Types.AnalysisStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AnalysisStatus
  ( AnalysisStatus
      ( ..,
        AnalysisStatus_COMPLETE,
        AnalysisStatus_FAILED,
        AnalysisStatus_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AnalysisStatus = AnalysisStatus'
  { fromAnalysisStatus ::
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

pattern AnalysisStatus_COMPLETE :: AnalysisStatus
pattern AnalysisStatus_COMPLETE = AnalysisStatus' "COMPLETE"

pattern AnalysisStatus_FAILED :: AnalysisStatus
pattern AnalysisStatus_FAILED = AnalysisStatus' "FAILED"

pattern AnalysisStatus_RUNNING :: AnalysisStatus
pattern AnalysisStatus_RUNNING = AnalysisStatus' "RUNNING"

{-# COMPLETE
  AnalysisStatus_COMPLETE,
  AnalysisStatus_FAILED,
  AnalysisStatus_RUNNING,
  AnalysisStatus'
  #-}
