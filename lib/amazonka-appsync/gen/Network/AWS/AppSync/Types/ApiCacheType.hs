{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ApiCacheType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.ApiCacheType
  ( ApiCacheType
    ( ApiCacheType'
    , ApiCacheTypeT2Small
    , ApiCacheTypeT2Medium
    , ApiCacheTypeR4Large
    , ApiCacheTypeR4Xlarge
    , ApiCacheTypeR42XLARGE
    , ApiCacheTypeR44XLARGE
    , ApiCacheTypeR48XLARGE
    , ApiCacheTypeSmall
    , ApiCacheTypeMedium
    , ApiCacheTypeLarge
    , ApiCacheTypeXlarge
    , ApiCacheTypeLarge2X
    , ApiCacheTypeLarge4X
    , ApiCacheTypeLarge8X
    , ApiCacheTypeLarge12X
    , fromApiCacheType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ApiCacheType = ApiCacheType'{fromApiCacheType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern ApiCacheTypeT2Small :: ApiCacheType
pattern ApiCacheTypeT2Small = ApiCacheType' "T2_SMALL"

pattern ApiCacheTypeT2Medium :: ApiCacheType
pattern ApiCacheTypeT2Medium = ApiCacheType' "T2_MEDIUM"

pattern ApiCacheTypeR4Large :: ApiCacheType
pattern ApiCacheTypeR4Large = ApiCacheType' "R4_LARGE"

pattern ApiCacheTypeR4Xlarge :: ApiCacheType
pattern ApiCacheTypeR4Xlarge = ApiCacheType' "R4_XLARGE"

pattern ApiCacheTypeR42XLARGE :: ApiCacheType
pattern ApiCacheTypeR42XLARGE = ApiCacheType' "R4_2XLARGE"

pattern ApiCacheTypeR44XLARGE :: ApiCacheType
pattern ApiCacheTypeR44XLARGE = ApiCacheType' "R4_4XLARGE"

pattern ApiCacheTypeR48XLARGE :: ApiCacheType
pattern ApiCacheTypeR48XLARGE = ApiCacheType' "R4_8XLARGE"

pattern ApiCacheTypeSmall :: ApiCacheType
pattern ApiCacheTypeSmall = ApiCacheType' "SMALL"

pattern ApiCacheTypeMedium :: ApiCacheType
pattern ApiCacheTypeMedium = ApiCacheType' "MEDIUM"

pattern ApiCacheTypeLarge :: ApiCacheType
pattern ApiCacheTypeLarge = ApiCacheType' "LARGE"

pattern ApiCacheTypeXlarge :: ApiCacheType
pattern ApiCacheTypeXlarge = ApiCacheType' "XLARGE"

pattern ApiCacheTypeLarge2X :: ApiCacheType
pattern ApiCacheTypeLarge2X = ApiCacheType' "LARGE_2X"

pattern ApiCacheTypeLarge4X :: ApiCacheType
pattern ApiCacheTypeLarge4X = ApiCacheType' "LARGE_4X"

pattern ApiCacheTypeLarge8X :: ApiCacheType
pattern ApiCacheTypeLarge8X = ApiCacheType' "LARGE_8X"

pattern ApiCacheTypeLarge12X :: ApiCacheType
pattern ApiCacheTypeLarge12X = ApiCacheType' "LARGE_12X"

{-# COMPLETE 
  ApiCacheTypeT2Small,

  ApiCacheTypeT2Medium,

  ApiCacheTypeR4Large,

  ApiCacheTypeR4Xlarge,

  ApiCacheTypeR42XLARGE,

  ApiCacheTypeR44XLARGE,

  ApiCacheTypeR48XLARGE,

  ApiCacheTypeSmall,

  ApiCacheTypeMedium,

  ApiCacheTypeLarge,

  ApiCacheTypeXlarge,

  ApiCacheTypeLarge2X,

  ApiCacheTypeLarge4X,

  ApiCacheTypeLarge8X,

  ApiCacheTypeLarge12X,
  ApiCacheType'
  #-}
