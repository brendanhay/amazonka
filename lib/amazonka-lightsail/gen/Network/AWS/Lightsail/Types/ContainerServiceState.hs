{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceState
  ( ContainerServiceState
      ( ContainerServiceState',
        CSSPending,
        CSSReady,
        CSSRunning,
        CSSUpdating,
        CSSDeleting,
        CSSDisabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContainerServiceState = ContainerServiceState' Lude.Text
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

pattern CSSPending :: ContainerServiceState
pattern CSSPending = ContainerServiceState' "PENDING"

pattern CSSReady :: ContainerServiceState
pattern CSSReady = ContainerServiceState' "READY"

pattern CSSRunning :: ContainerServiceState
pattern CSSRunning = ContainerServiceState' "RUNNING"

pattern CSSUpdating :: ContainerServiceState
pattern CSSUpdating = ContainerServiceState' "UPDATING"

pattern CSSDeleting :: ContainerServiceState
pattern CSSDeleting = ContainerServiceState' "DELETING"

pattern CSSDisabled :: ContainerServiceState
pattern CSSDisabled = ContainerServiceState' "DISABLED"

{-# COMPLETE
  CSSPending,
  CSSReady,
  CSSRunning,
  CSSUpdating,
  CSSDeleting,
  CSSDisabled,
  ContainerServiceState'
  #-}
