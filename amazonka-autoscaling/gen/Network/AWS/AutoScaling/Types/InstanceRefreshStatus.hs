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
-- Module      : Network.AWS.AutoScaling.Types.InstanceRefreshStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceRefreshStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype InstanceRefreshStatus = InstanceRefreshStatus'
  { fromInstanceRefreshStatus ::
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
