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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TrialComponentPrimaryStatus = TrialComponentPrimaryStatus'
  { fromTrialComponentPrimaryStatus ::
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
