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
-- Module      : Amazonka.SageMaker.Types.TransformJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TransformJobStatus
  ( TransformJobStatus
      ( ..,
        TransformJobStatus_Completed,
        TransformJobStatus_Failed,
        TransformJobStatus_InProgress,
        TransformJobStatus_Stopped,
        TransformJobStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TransformJobStatus = TransformJobStatus'
  { fromTransformJobStatus ::
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

pattern TransformJobStatus_Completed :: TransformJobStatus
pattern TransformJobStatus_Completed = TransformJobStatus' "Completed"

pattern TransformJobStatus_Failed :: TransformJobStatus
pattern TransformJobStatus_Failed = TransformJobStatus' "Failed"

pattern TransformJobStatus_InProgress :: TransformJobStatus
pattern TransformJobStatus_InProgress = TransformJobStatus' "InProgress"

pattern TransformJobStatus_Stopped :: TransformJobStatus
pattern TransformJobStatus_Stopped = TransformJobStatus' "Stopped"

pattern TransformJobStatus_Stopping :: TransformJobStatus
pattern TransformJobStatus_Stopping = TransformJobStatus' "Stopping"

{-# COMPLETE
  TransformJobStatus_Completed,
  TransformJobStatus_Failed,
  TransformJobStatus_InProgress,
  TransformJobStatus_Stopped,
  TransformJobStatus_Stopping,
  TransformJobStatus'
  #-}
