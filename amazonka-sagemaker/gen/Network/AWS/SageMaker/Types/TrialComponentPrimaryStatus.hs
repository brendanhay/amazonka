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
-- Module      : Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype TrialComponentPrimaryStatus = TrialComponentPrimaryStatus'
  { fromTrialComponentPrimaryStatus ::
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
