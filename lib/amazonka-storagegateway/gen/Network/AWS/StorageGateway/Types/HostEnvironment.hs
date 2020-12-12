{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.HostEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.HostEnvironment
  ( HostEnvironment
      ( HostEnvironment',
        EC2,
        HyperV,
        Kvm,
        Other,
        VMware
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HostEnvironment = HostEnvironment' Lude.Text
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

pattern EC2 :: HostEnvironment
pattern EC2 = HostEnvironment' "EC2"

pattern HyperV :: HostEnvironment
pattern HyperV = HostEnvironment' "HYPER-V"

pattern Kvm :: HostEnvironment
pattern Kvm = HostEnvironment' "KVM"

pattern Other :: HostEnvironment
pattern Other = HostEnvironment' "OTHER"

pattern VMware :: HostEnvironment
pattern VMware = HostEnvironment' "VMWARE"

{-# COMPLETE
  EC2,
  HyperV,
  Kvm,
  Other,
  VMware,
  HostEnvironment'
  #-}
