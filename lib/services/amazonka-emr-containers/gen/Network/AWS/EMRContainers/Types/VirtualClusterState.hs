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
-- Module      : Network.AWS.EMRContainers.Types.VirtualClusterState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMRContainers.Types.VirtualClusterState
  ( VirtualClusterState
      ( ..,
        VirtualClusterState_ARRESTED,
        VirtualClusterState_RUNNING,
        VirtualClusterState_TERMINATED,
        VirtualClusterState_TERMINATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype VirtualClusterState = VirtualClusterState'
  { fromVirtualClusterState ::
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

pattern VirtualClusterState_ARRESTED :: VirtualClusterState
pattern VirtualClusterState_ARRESTED = VirtualClusterState' "ARRESTED"

pattern VirtualClusterState_RUNNING :: VirtualClusterState
pattern VirtualClusterState_RUNNING = VirtualClusterState' "RUNNING"

pattern VirtualClusterState_TERMINATED :: VirtualClusterState
pattern VirtualClusterState_TERMINATED = VirtualClusterState' "TERMINATED"

pattern VirtualClusterState_TERMINATING :: VirtualClusterState
pattern VirtualClusterState_TERMINATING = VirtualClusterState' "TERMINATING"

{-# COMPLETE
  VirtualClusterState_ARRESTED,
  VirtualClusterState_RUNNING,
  VirtualClusterState_TERMINATED,
  VirtualClusterState_TERMINATING,
  VirtualClusterState'
  #-}
