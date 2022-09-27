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
-- Module      : Amazonka.DrS.Types.ReplicationConfigurationDataPlaneRouting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ReplicationConfigurationDataPlaneRouting
  ( ReplicationConfigurationDataPlaneRouting
      ( ..,
        ReplicationConfigurationDataPlaneRouting_PRIVATE_IP,
        ReplicationConfigurationDataPlaneRouting_PUBLIC_IP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ReplicationConfigurationDataPlaneRouting = ReplicationConfigurationDataPlaneRouting'
  { fromReplicationConfigurationDataPlaneRouting ::
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

pattern ReplicationConfigurationDataPlaneRouting_PRIVATE_IP :: ReplicationConfigurationDataPlaneRouting
pattern ReplicationConfigurationDataPlaneRouting_PRIVATE_IP = ReplicationConfigurationDataPlaneRouting' "PRIVATE_IP"

pattern ReplicationConfigurationDataPlaneRouting_PUBLIC_IP :: ReplicationConfigurationDataPlaneRouting
pattern ReplicationConfigurationDataPlaneRouting_PUBLIC_IP = ReplicationConfigurationDataPlaneRouting' "PUBLIC_IP"

{-# COMPLETE
  ReplicationConfigurationDataPlaneRouting_PRIVATE_IP,
  ReplicationConfigurationDataPlaneRouting_PUBLIC_IP,
  ReplicationConfigurationDataPlaneRouting'
  #-}
