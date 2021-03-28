{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OrderKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.OrderKey
  ( OrderKey
    ( OrderKey'
    , OrderKeyAscending
    , OrderKeyDescending
    , fromOrderKey
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OrderKey = OrderKey'{fromOrderKey :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern OrderKeyAscending :: OrderKey
pattern OrderKeyAscending = OrderKey' "Ascending"

pattern OrderKeyDescending :: OrderKey
pattern OrderKeyDescending = OrderKey' "Descending"

{-# COMPLETE 
  OrderKeyAscending,

  OrderKeyDescending,
  OrderKey'
  #-}
