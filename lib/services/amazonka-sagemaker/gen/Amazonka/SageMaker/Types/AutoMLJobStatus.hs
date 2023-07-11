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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobStatus
  ( AutoMLJobStatus
      ( ..,
        AutoMLJobStatus_Completed,
        AutoMLJobStatus_Failed,
        AutoMLJobStatus_InProgress,
        AutoMLJobStatus_Stopped,
        AutoMLJobStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoMLJobStatus = AutoMLJobStatus'
  { fromAutoMLJobStatus ::
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

pattern AutoMLJobStatus_Completed :: AutoMLJobStatus
pattern AutoMLJobStatus_Completed = AutoMLJobStatus' "Completed"

pattern AutoMLJobStatus_Failed :: AutoMLJobStatus
pattern AutoMLJobStatus_Failed = AutoMLJobStatus' "Failed"

pattern AutoMLJobStatus_InProgress :: AutoMLJobStatus
pattern AutoMLJobStatus_InProgress = AutoMLJobStatus' "InProgress"

pattern AutoMLJobStatus_Stopped :: AutoMLJobStatus
pattern AutoMLJobStatus_Stopped = AutoMLJobStatus' "Stopped"

pattern AutoMLJobStatus_Stopping :: AutoMLJobStatus
pattern AutoMLJobStatus_Stopping = AutoMLJobStatus' "Stopping"

{-# COMPLETE
  AutoMLJobStatus_Completed,
  AutoMLJobStatus_Failed,
  AutoMLJobStatus_InProgress,
  AutoMLJobStatus_Stopped,
  AutoMLJobStatus_Stopping,
  AutoMLJobStatus'
  #-}
