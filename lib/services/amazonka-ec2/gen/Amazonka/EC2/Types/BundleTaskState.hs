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
-- Module      : Amazonka.EC2.Types.BundleTaskState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.BundleTaskState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype BundleTaskState = BundleTaskState'
  { fromBundleTaskState ::
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
