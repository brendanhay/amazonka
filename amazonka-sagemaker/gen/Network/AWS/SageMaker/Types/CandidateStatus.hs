{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype CandidateStatus = CandidateStatus'
  { fromCandidateStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
