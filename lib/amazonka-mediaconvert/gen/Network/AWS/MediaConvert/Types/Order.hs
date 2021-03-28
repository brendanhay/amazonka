{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Order
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Order
  ( Order
    ( Order'
    , OrderAscending
    , OrderDescending
    , fromOrder
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
newtype Order = Order'{fromOrder :: Core.Text}
                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                  Core.Generic)
                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                    Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                    Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern OrderAscending :: Order
pattern OrderAscending = Order' "ASCENDING"

pattern OrderDescending :: Order
pattern OrderDescending = Order' "DESCENDING"

{-# COMPLETE 
  OrderAscending,

  OrderDescending,
  Order'
  #-}
