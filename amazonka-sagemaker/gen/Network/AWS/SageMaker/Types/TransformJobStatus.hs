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

import qualified Network.AWS.Prelude as Prelude

newtype TransformJobStatus = TransformJobStatus'
  { fromTransformJobStatus ::
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
