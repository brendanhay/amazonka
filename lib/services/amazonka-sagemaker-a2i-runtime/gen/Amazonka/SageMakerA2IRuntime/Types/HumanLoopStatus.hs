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
-- Module      : Amazonka.SageMakerA2IRuntime.Types.HumanLoopStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerA2IRuntime.Types.HumanLoopStatus
  ( HumanLoopStatus
      ( ..,
        HumanLoopStatus_Completed,
        HumanLoopStatus_Failed,
        HumanLoopStatus_InProgress,
        HumanLoopStatus_Stopped,
        HumanLoopStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HumanLoopStatus = HumanLoopStatus'
  { fromHumanLoopStatus ::
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

pattern HumanLoopStatus_Completed :: HumanLoopStatus
pattern HumanLoopStatus_Completed = HumanLoopStatus' "Completed"

pattern HumanLoopStatus_Failed :: HumanLoopStatus
pattern HumanLoopStatus_Failed = HumanLoopStatus' "Failed"

pattern HumanLoopStatus_InProgress :: HumanLoopStatus
pattern HumanLoopStatus_InProgress = HumanLoopStatus' "InProgress"

pattern HumanLoopStatus_Stopped :: HumanLoopStatus
pattern HumanLoopStatus_Stopped = HumanLoopStatus' "Stopped"

pattern HumanLoopStatus_Stopping :: HumanLoopStatus
pattern HumanLoopStatus_Stopping = HumanLoopStatus' "Stopping"

{-# COMPLETE
  HumanLoopStatus_Completed,
  HumanLoopStatus_Failed,
  HumanLoopStatus_InProgress,
  HumanLoopStatus_Stopped,
  HumanLoopStatus_Stopping,
  HumanLoopStatus'
  #-}
