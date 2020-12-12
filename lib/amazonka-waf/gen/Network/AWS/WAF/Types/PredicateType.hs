{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.PredicateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.PredicateType
  ( PredicateType
      ( PredicateType',
        ByteMatch,
        GeoMatch,
        IPMatch,
        RegexMatch,
        SizeConstraint,
        SqlInjectionMatch,
        XSSMatch
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PredicateType = PredicateType' Lude.Text
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

pattern ByteMatch :: PredicateType
pattern ByteMatch = PredicateType' "ByteMatch"

pattern GeoMatch :: PredicateType
pattern GeoMatch = PredicateType' "GeoMatch"

pattern IPMatch :: PredicateType
pattern IPMatch = PredicateType' "IPMatch"

pattern RegexMatch :: PredicateType
pattern RegexMatch = PredicateType' "RegexMatch"

pattern SizeConstraint :: PredicateType
pattern SizeConstraint = PredicateType' "SizeConstraint"

pattern SqlInjectionMatch :: PredicateType
pattern SqlInjectionMatch = PredicateType' "SqlInjectionMatch"

pattern XSSMatch :: PredicateType
pattern XSSMatch = PredicateType' "XssMatch"

{-# COMPLETE
  ByteMatch,
  GeoMatch,
  IPMatch,
  RegexMatch,
  SizeConstraint,
  SqlInjectionMatch,
  XSSMatch,
  PredicateType'
  #-}
