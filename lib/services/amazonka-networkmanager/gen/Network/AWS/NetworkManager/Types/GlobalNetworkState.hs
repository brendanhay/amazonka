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
-- Module      : Network.AWS.NetworkManager.Types.GlobalNetworkState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkManager.Types.GlobalNetworkState
  ( GlobalNetworkState
      ( ..,
        GlobalNetworkState_AVAILABLE,
        GlobalNetworkState_DELETING,
        GlobalNetworkState_PENDING,
        GlobalNetworkState_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype GlobalNetworkState = GlobalNetworkState'
  { fromGlobalNetworkState ::
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

pattern GlobalNetworkState_AVAILABLE :: GlobalNetworkState
pattern GlobalNetworkState_AVAILABLE = GlobalNetworkState' "AVAILABLE"

pattern GlobalNetworkState_DELETING :: GlobalNetworkState
pattern GlobalNetworkState_DELETING = GlobalNetworkState' "DELETING"

pattern GlobalNetworkState_PENDING :: GlobalNetworkState
pattern GlobalNetworkState_PENDING = GlobalNetworkState' "PENDING"

pattern GlobalNetworkState_UPDATING :: GlobalNetworkState
pattern GlobalNetworkState_UPDATING = GlobalNetworkState' "UPDATING"

{-# COMPLETE
  GlobalNetworkState_AVAILABLE,
  GlobalNetworkState_DELETING,
  GlobalNetworkState_PENDING,
  GlobalNetworkState_UPDATING,
  GlobalNetworkState'
  #-}
