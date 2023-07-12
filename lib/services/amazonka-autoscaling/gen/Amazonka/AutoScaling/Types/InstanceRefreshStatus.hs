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
-- Module      : Amazonka.AutoScaling.Types.InstanceRefreshStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceRefreshStatus
  ( InstanceRefreshStatus
      ( ..,
        InstanceRefreshStatus_Cancelled,
        InstanceRefreshStatus_Cancelling,
        InstanceRefreshStatus_Failed,
        InstanceRefreshStatus_InProgress,
        InstanceRefreshStatus_Pending,
        InstanceRefreshStatus_Successful
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceRefreshStatus = InstanceRefreshStatus'
  { fromInstanceRefreshStatus ::
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

pattern InstanceRefreshStatus_Cancelled :: InstanceRefreshStatus
pattern InstanceRefreshStatus_Cancelled = InstanceRefreshStatus' "Cancelled"

pattern InstanceRefreshStatus_Cancelling :: InstanceRefreshStatus
pattern InstanceRefreshStatus_Cancelling = InstanceRefreshStatus' "Cancelling"

pattern InstanceRefreshStatus_Failed :: InstanceRefreshStatus
pattern InstanceRefreshStatus_Failed = InstanceRefreshStatus' "Failed"

pattern InstanceRefreshStatus_InProgress :: InstanceRefreshStatus
pattern InstanceRefreshStatus_InProgress = InstanceRefreshStatus' "InProgress"

pattern InstanceRefreshStatus_Pending :: InstanceRefreshStatus
pattern InstanceRefreshStatus_Pending = InstanceRefreshStatus' "Pending"

pattern InstanceRefreshStatus_Successful :: InstanceRefreshStatus
pattern InstanceRefreshStatus_Successful = InstanceRefreshStatus' "Successful"

{-# COMPLETE
  InstanceRefreshStatus_Cancelled,
  InstanceRefreshStatus_Cancelling,
  InstanceRefreshStatus_Failed,
  InstanceRefreshStatus_InProgress,
  InstanceRefreshStatus_Pending,
  InstanceRefreshStatus_Successful,
  InstanceRefreshStatus'
  #-}
