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
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AutoSnapshotStatus
  ( AutoSnapshotStatus
      ( ..,
        AutoSnapshotStatus_Failed,
        AutoSnapshotStatus_InProgress,
        AutoSnapshotStatus_NotFound,
        AutoSnapshotStatus_Success
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AutoSnapshotStatus = AutoSnapshotStatus'
  { fromAutoSnapshotStatus ::
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

pattern AutoSnapshotStatus_Failed :: AutoSnapshotStatus
pattern AutoSnapshotStatus_Failed = AutoSnapshotStatus' "Failed"

pattern AutoSnapshotStatus_InProgress :: AutoSnapshotStatus
pattern AutoSnapshotStatus_InProgress = AutoSnapshotStatus' "InProgress"

pattern AutoSnapshotStatus_NotFound :: AutoSnapshotStatus
pattern AutoSnapshotStatus_NotFound = AutoSnapshotStatus' "NotFound"

pattern AutoSnapshotStatus_Success :: AutoSnapshotStatus
pattern AutoSnapshotStatus_Success = AutoSnapshotStatus' "Success"

{-# COMPLETE
  AutoSnapshotStatus_Failed,
  AutoSnapshotStatus_InProgress,
  AutoSnapshotStatus_NotFound,
  AutoSnapshotStatus_Success,
  AutoSnapshotStatus'
  #-}
