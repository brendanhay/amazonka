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
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshotState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceSnapshotState
  ( InstanceSnapshotState
      ( ..,
        InstanceSnapshotState_Available,
        InstanceSnapshotState_Error,
        InstanceSnapshotState_Pending
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype InstanceSnapshotState = InstanceSnapshotState'
  { fromInstanceSnapshotState ::
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

pattern InstanceSnapshotState_Available :: InstanceSnapshotState
pattern InstanceSnapshotState_Available = InstanceSnapshotState' "available"

pattern InstanceSnapshotState_Error :: InstanceSnapshotState
pattern InstanceSnapshotState_Error = InstanceSnapshotState' "error"

pattern InstanceSnapshotState_Pending :: InstanceSnapshotState
pattern InstanceSnapshotState_Pending = InstanceSnapshotState' "pending"

{-# COMPLETE
  InstanceSnapshotState_Available,
  InstanceSnapshotState_Error,
  InstanceSnapshotState_Pending,
  InstanceSnapshotState'
  #-}
