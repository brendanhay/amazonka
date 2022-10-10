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
-- Module      : Amazonka.SageMaker.Types.TrialComponentPrimaryStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentPrimaryStatus
  ( TrialComponentPrimaryStatus
      ( ..,
        TrialComponentPrimaryStatus_Completed,
        TrialComponentPrimaryStatus_Failed,
        TrialComponentPrimaryStatus_InProgress,
        TrialComponentPrimaryStatus_Stopped,
        TrialComponentPrimaryStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TrialComponentPrimaryStatus = TrialComponentPrimaryStatus'
  { fromTrialComponentPrimaryStatus ::
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

pattern TrialComponentPrimaryStatus_Completed :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatus_Completed = TrialComponentPrimaryStatus' "Completed"

pattern TrialComponentPrimaryStatus_Failed :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatus_Failed = TrialComponentPrimaryStatus' "Failed"

pattern TrialComponentPrimaryStatus_InProgress :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatus_InProgress = TrialComponentPrimaryStatus' "InProgress"

pattern TrialComponentPrimaryStatus_Stopped :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatus_Stopped = TrialComponentPrimaryStatus' "Stopped"

pattern TrialComponentPrimaryStatus_Stopping :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatus_Stopping = TrialComponentPrimaryStatus' "Stopping"

{-# COMPLETE
  TrialComponentPrimaryStatus_Completed,
  TrialComponentPrimaryStatus_Failed,
  TrialComponentPrimaryStatus_InProgress,
  TrialComponentPrimaryStatus_Stopped,
  TrialComponentPrimaryStatus_Stopping,
  TrialComponentPrimaryStatus'
  #-}
