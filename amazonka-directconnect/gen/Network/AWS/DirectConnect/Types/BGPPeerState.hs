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
-- Module      : Network.AWS.DirectConnect.Types.BGPPeerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.BGPPeerState
  ( BGPPeerState
      ( ..,
        BGPPeerState_Available,
        BGPPeerState_Deleted,
        BGPPeerState_Deleting,
        BGPPeerState_Pending,
        BGPPeerState_Verifying
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype BGPPeerState = BGPPeerState'
  { fromBGPPeerState ::
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

pattern BGPPeerState_Available :: BGPPeerState
pattern BGPPeerState_Available = BGPPeerState' "available"

pattern BGPPeerState_Deleted :: BGPPeerState
pattern BGPPeerState_Deleted = BGPPeerState' "deleted"

pattern BGPPeerState_Deleting :: BGPPeerState
pattern BGPPeerState_Deleting = BGPPeerState' "deleting"

pattern BGPPeerState_Pending :: BGPPeerState
pattern BGPPeerState_Pending = BGPPeerState' "pending"

pattern BGPPeerState_Verifying :: BGPPeerState
pattern BGPPeerState_Verifying = BGPPeerState' "verifying"

{-# COMPLETE
  BGPPeerState_Available,
  BGPPeerState_Deleted,
  BGPPeerState_Deleting,
  BGPPeerState_Pending,
  BGPPeerState_Verifying,
  BGPPeerState'
  #-}
