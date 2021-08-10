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
-- Module      : Network.AWS.DirectConnect.Types.InterconnectState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.InterconnectState
  ( InterconnectState
      ( ..,
        InterconnectState_Available,
        InterconnectState_Deleted,
        InterconnectState_Deleting,
        InterconnectState_Down,
        InterconnectState_Pending,
        InterconnectState_Requested,
        InterconnectState_Unknown
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InterconnectState = InterconnectState'
  { fromInterconnectState ::
      Core.Text
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

pattern InterconnectState_Available :: InterconnectState
pattern InterconnectState_Available = InterconnectState' "available"

pattern InterconnectState_Deleted :: InterconnectState
pattern InterconnectState_Deleted = InterconnectState' "deleted"

pattern InterconnectState_Deleting :: InterconnectState
pattern InterconnectState_Deleting = InterconnectState' "deleting"

pattern InterconnectState_Down :: InterconnectState
pattern InterconnectState_Down = InterconnectState' "down"

pattern InterconnectState_Pending :: InterconnectState
pattern InterconnectState_Pending = InterconnectState' "pending"

pattern InterconnectState_Requested :: InterconnectState
pattern InterconnectState_Requested = InterconnectState' "requested"

pattern InterconnectState_Unknown :: InterconnectState
pattern InterconnectState_Unknown = InterconnectState' "unknown"

{-# COMPLETE
  InterconnectState_Available,
  InterconnectState_Deleted,
  InterconnectState_Deleting,
  InterconnectState_Down,
  InterconnectState_Pending,
  InterconnectState_Requested,
  InterconnectState_Unknown,
  InterconnectState'
  #-}
