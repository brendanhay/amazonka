{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PriceClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.PriceClass
  ( PriceClass
    ( PriceClass'
    , PriceClassPriceClass100
    , PriceClassPriceClass200
    , PriceClassPriceClassAll
    , fromPriceClass
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PriceClass = PriceClass'{fromPriceClass :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern PriceClassPriceClass100 :: PriceClass
pattern PriceClassPriceClass100 = PriceClass' "PriceClass_100"

pattern PriceClassPriceClass200 :: PriceClass
pattern PriceClassPriceClass200 = PriceClass' "PriceClass_200"

pattern PriceClassPriceClassAll :: PriceClass
pattern PriceClassPriceClassAll = PriceClass' "PriceClass_All"

{-# COMPLETE 
  PriceClassPriceClass100,

  PriceClassPriceClass200,

  PriceClassPriceClassAll,
  PriceClass'
  #-}
