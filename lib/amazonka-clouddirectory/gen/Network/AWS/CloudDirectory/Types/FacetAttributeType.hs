{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        FacetAttributeTypeString,
        FacetAttributeTypeBinary,
        FacetAttributeTypeBoolean,
        FacetAttributeTypeNumber,
        FacetAttributeTypeDatetime,
        FacetAttributeTypeVariant,
        fromFacetAttributeType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype FacetAttributeType = FacetAttributeType'
  { fromFacetAttributeType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern FacetAttributeTypeString :: FacetAttributeType
pattern FacetAttributeTypeString = FacetAttributeType' "STRING"

pattern FacetAttributeTypeBinary :: FacetAttributeType
pattern FacetAttributeTypeBinary = FacetAttributeType' "BINARY"

pattern FacetAttributeTypeBoolean :: FacetAttributeType
pattern FacetAttributeTypeBoolean = FacetAttributeType' "BOOLEAN"

pattern FacetAttributeTypeNumber :: FacetAttributeType
pattern FacetAttributeTypeNumber = FacetAttributeType' "NUMBER"

pattern FacetAttributeTypeDatetime :: FacetAttributeType
pattern FacetAttributeTypeDatetime = FacetAttributeType' "DATETIME"

pattern FacetAttributeTypeVariant :: FacetAttributeType
pattern FacetAttributeTypeVariant = FacetAttributeType' "VARIANT"

{-# COMPLETE
  FacetAttributeTypeString,
  FacetAttributeTypeBinary,
  FacetAttributeTypeBoolean,
  FacetAttributeTypeNumber,
  FacetAttributeTypeDatetime,
  FacetAttributeTypeVariant,
  FacetAttributeType'
  #-}
