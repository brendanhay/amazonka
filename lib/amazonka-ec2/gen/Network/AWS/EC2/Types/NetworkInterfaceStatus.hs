-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceStatus
  ( NetworkInterfaceStatus
      ( NetworkInterfaceStatus',
        NISAssociated,
        NISAttaching,
        NISAvailable,
        NISDetaching,
        NISInUse
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NetworkInterfaceStatus = NetworkInterfaceStatus' Lude.Text
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

pattern NISAssociated :: NetworkInterfaceStatus
pattern NISAssociated = NetworkInterfaceStatus' "associated"

pattern NISAttaching :: NetworkInterfaceStatus
pattern NISAttaching = NetworkInterfaceStatus' "attaching"

pattern NISAvailable :: NetworkInterfaceStatus
pattern NISAvailable = NetworkInterfaceStatus' "available"

pattern NISDetaching :: NetworkInterfaceStatus
pattern NISDetaching = NetworkInterfaceStatus' "detaching"

pattern NISInUse :: NetworkInterfaceStatus
pattern NISInUse = NetworkInterfaceStatus' "in-use"

{-# COMPLETE
  NISAssociated,
  NISAttaching,
  NISAvailable,
  NISDetaching,
  NISInUse,
  NetworkInterfaceStatus'
  #-}
