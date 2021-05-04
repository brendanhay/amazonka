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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionState
  ( ConnectionState
      ( ..,
        ConnectionState_AUTHORIZED,
        ConnectionState_AUTHORIZING,
        ConnectionState_CREATING,
        ConnectionState_DEAUTHORIZED,
        ConnectionState_DEAUTHORIZING,
        ConnectionState_DELETING,
        ConnectionState_UPDATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ConnectionState = ConnectionState'
  { fromConnectionState ::
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

pattern ConnectionState_AUTHORIZED :: ConnectionState
pattern ConnectionState_AUTHORIZED = ConnectionState' "AUTHORIZED"

pattern ConnectionState_AUTHORIZING :: ConnectionState
pattern ConnectionState_AUTHORIZING = ConnectionState' "AUTHORIZING"

pattern ConnectionState_CREATING :: ConnectionState
pattern ConnectionState_CREATING = ConnectionState' "CREATING"

pattern ConnectionState_DEAUTHORIZED :: ConnectionState
pattern ConnectionState_DEAUTHORIZED = ConnectionState' "DEAUTHORIZED"

pattern ConnectionState_DEAUTHORIZING :: ConnectionState
pattern ConnectionState_DEAUTHORIZING = ConnectionState' "DEAUTHORIZING"

pattern ConnectionState_DELETING :: ConnectionState
pattern ConnectionState_DELETING = ConnectionState' "DELETING"

pattern ConnectionState_UPDATING :: ConnectionState
pattern ConnectionState_UPDATING = ConnectionState' "UPDATING"

{-# COMPLETE
  ConnectionState_AUTHORIZED,
  ConnectionState_AUTHORIZING,
  ConnectionState_CREATING,
  ConnectionState_DEAUTHORIZED,
  ConnectionState_DEAUTHORIZING,
  ConnectionState_DELETING,
  ConnectionState_UPDATING,
  ConnectionState'
  #-}
