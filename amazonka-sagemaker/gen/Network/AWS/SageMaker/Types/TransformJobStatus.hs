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
-- Module      : Network.AWS.SageMaker.Types.TransformJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJobStatus
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

import qualified Network.AWS.Core as Core

newtype TransformJobStatus = TransformJobStatus'
  { fromTransformJobStatus ::
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
