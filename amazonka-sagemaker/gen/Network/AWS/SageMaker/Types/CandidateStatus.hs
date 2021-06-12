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
-- Module      : Network.AWS.SageMaker.Types.CandidateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CandidateStatus
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

import qualified Network.AWS.Core as Core

newtype CandidateStatus = CandidateStatus'
  { fromCandidateStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
