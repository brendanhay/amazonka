-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeType
  ( FacetAttributeType
      ( FacetAttributeType',
        Binary,
        Boolean,
        Datetime,
        Number,
        String,
        Variant
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FacetAttributeType = FacetAttributeType' Lude.Text
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

pattern Binary :: FacetAttributeType
pattern Binary = FacetAttributeType' "BINARY"

pattern Boolean :: FacetAttributeType
pattern Boolean = FacetAttributeType' "BOOLEAN"

pattern Datetime :: FacetAttributeType
pattern Datetime = FacetAttributeType' "DATETIME"

pattern Number :: FacetAttributeType
pattern Number = FacetAttributeType' "NUMBER"

pattern String :: FacetAttributeType
pattern String = FacetAttributeType' "STRING"

pattern Variant :: FacetAttributeType
pattern Variant = FacetAttributeType' "VARIANT"

{-# COMPLETE
  Binary,
  Boolean,
  Datetime,
  Number,
  String,
  Variant,
  FacetAttributeType'
  #-}
