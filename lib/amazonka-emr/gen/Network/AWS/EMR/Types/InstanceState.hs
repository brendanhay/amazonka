{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceState
  ( InstanceState
      ( InstanceState',
        ISAwaitingFulfillment,
        ISProvisioning,
        ISBootstrapping,
        ISRunning,
        ISTerminated
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceState = InstanceState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ISAwaitingFulfillment :: InstanceState
pattern ISAwaitingFulfillment = InstanceState' "AWAITING_FULFILLMENT"

pattern ISProvisioning :: InstanceState
pattern ISProvisioning = InstanceState' "PROVISIONING"

pattern ISBootstrapping :: InstanceState
pattern ISBootstrapping = InstanceState' "BOOTSTRAPPING"

pattern ISRunning :: InstanceState
pattern ISRunning = InstanceState' "RUNNING"

pattern ISTerminated :: InstanceState
pattern ISTerminated = InstanceState' "TERMINATED"

{-# COMPLETE
  ISAwaitingFulfillment,
  ISProvisioning,
  ISBootstrapping,
  ISRunning,
  ISTerminated,
  InstanceState'
  #-}
