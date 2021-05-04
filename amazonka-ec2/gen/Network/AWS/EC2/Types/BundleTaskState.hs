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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype BundleTaskState = BundleTaskState'
  { fromBundleTaskState ::
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
