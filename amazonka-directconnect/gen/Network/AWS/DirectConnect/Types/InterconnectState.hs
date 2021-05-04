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

import qualified Network.AWS.Prelude as Prelude

newtype InterconnectState = InterconnectState'
  { fromInterconnectState ::
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
