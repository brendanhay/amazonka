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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkState
  ( CoreNetworkState
      ( ..,
        CoreNetworkState_AVAILABLE,
        CoreNetworkState_CREATING,
        CoreNetworkState_DELETING,
        CoreNetworkState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CoreNetworkState = CoreNetworkState'
  { fromCoreNetworkState ::
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

pattern CoreNetworkState_AVAILABLE :: CoreNetworkState
pattern CoreNetworkState_AVAILABLE = CoreNetworkState' "AVAILABLE"

pattern CoreNetworkState_CREATING :: CoreNetworkState
pattern CoreNetworkState_CREATING = CoreNetworkState' "CREATING"

pattern CoreNetworkState_DELETING :: CoreNetworkState
pattern CoreNetworkState_DELETING = CoreNetworkState' "DELETING"

pattern CoreNetworkState_UPDATING :: CoreNetworkState
pattern CoreNetworkState_UPDATING = CoreNetworkState' "UPDATING"

{-# COMPLETE
  CoreNetworkState_AVAILABLE,
  CoreNetworkState_CREATING,
  CoreNetworkState_DELETING,
  CoreNetworkState_UPDATING,
  CoreNetworkState'
  #-}
