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
-- Module      : Network.AWS.SageMaker.Types.DetailedAlgorithmStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DetailedAlgorithmStatus
  ( DetailedAlgorithmStatus
      ( ..,
        DetailedAlgorithmStatus_Completed,
        DetailedAlgorithmStatus_Failed,
        DetailedAlgorithmStatus_InProgress,
        DetailedAlgorithmStatus_NotStarted
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DetailedAlgorithmStatus = DetailedAlgorithmStatus'
  { fromDetailedAlgorithmStatus ::
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

pattern DetailedAlgorithmStatus_Completed :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_Completed = DetailedAlgorithmStatus' "Completed"

pattern DetailedAlgorithmStatus_Failed :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_Failed = DetailedAlgorithmStatus' "Failed"

pattern DetailedAlgorithmStatus_InProgress :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_InProgress = DetailedAlgorithmStatus' "InProgress"

pattern DetailedAlgorithmStatus_NotStarted :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_NotStarted = DetailedAlgorithmStatus' "NotStarted"

{-# COMPLETE
  DetailedAlgorithmStatus_Completed,
  DetailedAlgorithmStatus_Failed,
  DetailedAlgorithmStatus_InProgress,
  DetailedAlgorithmStatus_NotStarted,
  DetailedAlgorithmStatus'
  #-}
