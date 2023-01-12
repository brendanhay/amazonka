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
-- Module      : Amazonka.SageMaker.Types.CandidateStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CandidateStatus
  ( CandidateStatus
      ( ..,
        CandidateStatus_Completed,
        CandidateStatus_Failed,
        CandidateStatus_InProgress,
        CandidateStatus_Stopped,
        CandidateStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CandidateStatus = CandidateStatus'
  { fromCandidateStatus ::
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

pattern CandidateStatus_Completed :: CandidateStatus
pattern CandidateStatus_Completed = CandidateStatus' "Completed"

pattern CandidateStatus_Failed :: CandidateStatus
pattern CandidateStatus_Failed = CandidateStatus' "Failed"

pattern CandidateStatus_InProgress :: CandidateStatus
pattern CandidateStatus_InProgress = CandidateStatus' "InProgress"

pattern CandidateStatus_Stopped :: CandidateStatus
pattern CandidateStatus_Stopped = CandidateStatus' "Stopped"

pattern CandidateStatus_Stopping :: CandidateStatus
pattern CandidateStatus_Stopping = CandidateStatus' "Stopping"

{-# COMPLETE
  CandidateStatus_Completed,
  CandidateStatus_Failed,
  CandidateStatus_InProgress,
  CandidateStatus_Stopped,
  CandidateStatus_Stopping,
  CandidateStatus'
  #-}
