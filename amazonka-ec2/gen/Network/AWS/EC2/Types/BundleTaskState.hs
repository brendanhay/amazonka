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
-- Module      : Network.AWS.EC2.Types.BundleTaskState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BundleTaskState
  ( BundleTaskState
      ( ..,
        BundleTaskState_Bundling,
        BundleTaskState_Cancelling,
        BundleTaskState_Complete,
        BundleTaskState_Failed,
        BundleTaskState_Pending,
        BundleTaskState_Storing,
        BundleTaskState_Waiting_for_shutdown
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype BundleTaskState = BundleTaskState'
  { fromBundleTaskState ::
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

pattern BundleTaskState_Bundling :: BundleTaskState
pattern BundleTaskState_Bundling = BundleTaskState' "bundling"

pattern BundleTaskState_Cancelling :: BundleTaskState
pattern BundleTaskState_Cancelling = BundleTaskState' "cancelling"

pattern BundleTaskState_Complete :: BundleTaskState
pattern BundleTaskState_Complete = BundleTaskState' "complete"

pattern BundleTaskState_Failed :: BundleTaskState
pattern BundleTaskState_Failed = BundleTaskState' "failed"

pattern BundleTaskState_Pending :: BundleTaskState
pattern BundleTaskState_Pending = BundleTaskState' "pending"

pattern BundleTaskState_Storing :: BundleTaskState
pattern BundleTaskState_Storing = BundleTaskState' "storing"

pattern BundleTaskState_Waiting_for_shutdown :: BundleTaskState
pattern BundleTaskState_Waiting_for_shutdown = BundleTaskState' "waiting-for-shutdown"

{-# COMPLETE
  BundleTaskState_Bundling,
  BundleTaskState_Cancelling,
  BundleTaskState_Complete,
  BundleTaskState_Failed,
  BundleTaskState_Pending,
  BundleTaskState_Storing,
  BundleTaskState_Waiting_for_shutdown,
  BundleTaskState'
  #-}
