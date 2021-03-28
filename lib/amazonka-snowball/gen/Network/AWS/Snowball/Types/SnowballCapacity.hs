{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.SnowballCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.SnowballCapacity
  ( SnowballCapacity
    ( SnowballCapacity'
    , SnowballCapacityT50
    , SnowballCapacityT80
    , SnowballCapacityT100
    , SnowballCapacityT42
    , SnowballCapacityT98
    , SnowballCapacityT8
    , SnowballCapacityNoPreference
    , fromSnowballCapacity
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SnowballCapacity = SnowballCapacity'{fromSnowballCapacity
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern SnowballCapacityT50 :: SnowballCapacity
pattern SnowballCapacityT50 = SnowballCapacity' "T50"

pattern SnowballCapacityT80 :: SnowballCapacity
pattern SnowballCapacityT80 = SnowballCapacity' "T80"

pattern SnowballCapacityT100 :: SnowballCapacity
pattern SnowballCapacityT100 = SnowballCapacity' "T100"

pattern SnowballCapacityT42 :: SnowballCapacity
pattern SnowballCapacityT42 = SnowballCapacity' "T42"

pattern SnowballCapacityT98 :: SnowballCapacity
pattern SnowballCapacityT98 = SnowballCapacity' "T98"

pattern SnowballCapacityT8 :: SnowballCapacity
pattern SnowballCapacityT8 = SnowballCapacity' "T8"

pattern SnowballCapacityNoPreference :: SnowballCapacity
pattern SnowballCapacityNoPreference = SnowballCapacity' "NoPreference"

{-# COMPLETE 
  SnowballCapacityT50,

  SnowballCapacityT80,

  SnowballCapacityT100,

  SnowballCapacityT42,

  SnowballCapacityT98,

  SnowballCapacityT8,

  SnowballCapacityNoPreference,
  SnowballCapacity'
  #-}
