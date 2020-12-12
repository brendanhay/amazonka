{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SimulateReservedQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SimulateReservedQueue
  ( SimulateReservedQueue
      ( SimulateReservedQueue',
        SRQDisabled,
        SRQEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
newtype SimulateReservedQueue = SimulateReservedQueue' Lude.Text
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

pattern SRQDisabled :: SimulateReservedQueue
pattern SRQDisabled = SimulateReservedQueue' "DISABLED"

pattern SRQEnabled :: SimulateReservedQueue
pattern SRQEnabled = SimulateReservedQueue' "ENABLED"

{-# COMPLETE
  SRQDisabled,
  SRQEnabled,
  SimulateReservedQueue'
  #-}
