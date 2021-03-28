{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.PredicateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.PredicateType
  ( PredicateType
    ( PredicateType'
    , PredicateTypeIPMatch
    , PredicateTypeByteMatch
    , PredicateTypeSqlInjectionMatch
    , PredicateTypeGeoMatch
    , PredicateTypeSizeConstraint
    , PredicateTypeXssMatch
    , PredicateTypeRegexMatch
    , fromPredicateType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PredicateType = PredicateType'{fromPredicateType ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern PredicateTypeIPMatch :: PredicateType
pattern PredicateTypeIPMatch = PredicateType' "IPMatch"

pattern PredicateTypeByteMatch :: PredicateType
pattern PredicateTypeByteMatch = PredicateType' "ByteMatch"

pattern PredicateTypeSqlInjectionMatch :: PredicateType
pattern PredicateTypeSqlInjectionMatch = PredicateType' "SqlInjectionMatch"

pattern PredicateTypeGeoMatch :: PredicateType
pattern PredicateTypeGeoMatch = PredicateType' "GeoMatch"

pattern PredicateTypeSizeConstraint :: PredicateType
pattern PredicateTypeSizeConstraint = PredicateType' "SizeConstraint"

pattern PredicateTypeXssMatch :: PredicateType
pattern PredicateTypeXssMatch = PredicateType' "XssMatch"

pattern PredicateTypeRegexMatch :: PredicateType
pattern PredicateTypeRegexMatch = PredicateType' "RegexMatch"

{-# COMPLETE 
  PredicateTypeIPMatch,

  PredicateTypeByteMatch,

  PredicateTypeSqlInjectionMatch,

  PredicateTypeGeoMatch,

  PredicateTypeSizeConstraint,

  PredicateTypeXssMatch,

  PredicateTypeRegexMatch,
  PredicateType'
  #-}
