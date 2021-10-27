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
-- Module      : Network.AWS.Route53Resolver.Types.ResolverEndpointStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Resolver.Types.ResolverEndpointStatus
  ( ResolverEndpointStatus
      ( ..,
        ResolverEndpointStatus_ACTION_NEEDED,
        ResolverEndpointStatus_AUTO_RECOVERING,
        ResolverEndpointStatus_CREATING,
        ResolverEndpointStatus_DELETING,
        ResolverEndpointStatus_OPERATIONAL,
        ResolverEndpointStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ResolverEndpointStatus = ResolverEndpointStatus'
  { fromResolverEndpointStatus ::
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

pattern ResolverEndpointStatus_ACTION_NEEDED :: ResolverEndpointStatus
pattern ResolverEndpointStatus_ACTION_NEEDED = ResolverEndpointStatus' "ACTION_NEEDED"

pattern ResolverEndpointStatus_AUTO_RECOVERING :: ResolverEndpointStatus
pattern ResolverEndpointStatus_AUTO_RECOVERING = ResolverEndpointStatus' "AUTO_RECOVERING"

pattern ResolverEndpointStatus_CREATING :: ResolverEndpointStatus
pattern ResolverEndpointStatus_CREATING = ResolverEndpointStatus' "CREATING"

pattern ResolverEndpointStatus_DELETING :: ResolverEndpointStatus
pattern ResolverEndpointStatus_DELETING = ResolverEndpointStatus' "DELETING"

pattern ResolverEndpointStatus_OPERATIONAL :: ResolverEndpointStatus
pattern ResolverEndpointStatus_OPERATIONAL = ResolverEndpointStatus' "OPERATIONAL"

pattern ResolverEndpointStatus_UPDATING :: ResolverEndpointStatus
pattern ResolverEndpointStatus_UPDATING = ResolverEndpointStatus' "UPDATING"

{-# COMPLETE
  ResolverEndpointStatus_ACTION_NEEDED,
  ResolverEndpointStatus_AUTO_RECOVERING,
  ResolverEndpointStatus_CREATING,
  ResolverEndpointStatus_DELETING,
  ResolverEndpointStatus_OPERATIONAL,
  ResolverEndpointStatus_UPDATING,
  ResolverEndpointStatus'
  #-}
