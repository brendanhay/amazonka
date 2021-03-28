{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationTenancy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CapacityReservationTenancy
  ( CapacityReservationTenancy
    ( CapacityReservationTenancy'
    , CapacityReservationTenancyDefault
    , CapacityReservationTenancyDedicated
    , fromCapacityReservationTenancy
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CapacityReservationTenancy = CapacityReservationTenancy'{fromCapacityReservationTenancy
                                                                 :: Core.Text}
                                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                       Core.Generic)
                                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                         Core.ToJSONKey, Core.FromJSONKey,
                                                         Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                         Core.FromXML, Core.ToText, Core.FromText,
                                                         Core.ToByteString, Core.ToQuery,
                                                         Core.ToHeader)

pattern CapacityReservationTenancyDefault :: CapacityReservationTenancy
pattern CapacityReservationTenancyDefault = CapacityReservationTenancy' "default"

pattern CapacityReservationTenancyDedicated :: CapacityReservationTenancy
pattern CapacityReservationTenancyDedicated = CapacityReservationTenancy' "dedicated"

{-# COMPLETE 
  CapacityReservationTenancyDefault,

  CapacityReservationTenancyDedicated,
  CapacityReservationTenancy'
  #-}
