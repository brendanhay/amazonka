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

import qualified Network.AWS.Prelude as Prelude

newtype AutoSnapshotStatus = AutoSnapshotStatus'
  { fromAutoSnapshotStatus ::
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
