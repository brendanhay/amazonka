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
        HostEnvironmentVmware,
        HostEnvironmentHyperV,
        HostEnvironmentEC2,
        HostEnvironmentKvm,
        HostEnvironmentOther,
        fromHostEnvironment
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype HostEnvironment = HostEnvironment'
  { fromHostEnvironment ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern HostEnvironmentVmware :: HostEnvironment
pattern HostEnvironmentVmware = HostEnvironment' "VMWARE"

pattern HostEnvironmentHyperV :: HostEnvironment
pattern HostEnvironmentHyperV = HostEnvironment' "HYPER-V"

pattern HostEnvironmentEC2 :: HostEnvironment
pattern HostEnvironmentEC2 = HostEnvironment' "EC2"

pattern HostEnvironmentKvm :: HostEnvironment
pattern HostEnvironmentKvm = HostEnvironment' "KVM"

pattern HostEnvironmentOther :: HostEnvironment
pattern HostEnvironmentOther = HostEnvironment' "OTHER"

{-# COMPLETE
  HostEnvironmentVmware,
  HostEnvironmentHyperV,
  HostEnvironmentEC2,
  HostEnvironmentKvm,
  HostEnvironmentOther,
  HostEnvironment'
  #-}
