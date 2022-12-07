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
-- Module      : Amazonka.EMRContainers.Types.VirtualClusterState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.VirtualClusterState
  ( VirtualClusterState
      ( ..,
        VirtualClusterState_ARRESTED,
        VirtualClusterState_RUNNING,
        VirtualClusterState_TERMINATED,
        VirtualClusterState_TERMINATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VirtualClusterState = VirtualClusterState'
  { fromVirtualClusterState ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
