{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpnEcmpSupportValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpnEcmpSupportValue
  ( VpnEcmpSupportValue
    ( VpnEcmpSupportValue'
    , VpnEcmpSupportValueEnable
    , VpnEcmpSupportValueDisable
    , fromVpnEcmpSupportValue
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype VpnEcmpSupportValue = VpnEcmpSupportValue'{fromVpnEcmpSupportValue
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern VpnEcmpSupportValueEnable :: VpnEcmpSupportValue
pattern VpnEcmpSupportValueEnable = VpnEcmpSupportValue' "enable"

pattern VpnEcmpSupportValueDisable :: VpnEcmpSupportValue
pattern VpnEcmpSupportValueDisable = VpnEcmpSupportValue' "disable"

{-# COMPLETE 
  VpnEcmpSupportValueEnable,

  VpnEcmpSupportValueDisable,
  VpnEcmpSupportValue'
  #-}
