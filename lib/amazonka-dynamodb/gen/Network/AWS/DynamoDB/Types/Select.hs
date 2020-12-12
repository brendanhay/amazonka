{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Select
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Select
  ( Select
      ( Select',
        AllAttributes,
        AllProjectedAttributes,
        Count,
        SpecificAttributes
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Select = Select' Lude.Text
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

pattern AllAttributes :: Select
pattern AllAttributes = Select' "ALL_ATTRIBUTES"

pattern AllProjectedAttributes :: Select
pattern AllProjectedAttributes = Select' "ALL_PROJECTED_ATTRIBUTES"

pattern Count :: Select
pattern Count = Select' "COUNT"

pattern SpecificAttributes :: Select
pattern SpecificAttributes = Select' "SPECIFIC_ATTRIBUTES"

{-# COMPLETE
  AllAttributes,
  AllProjectedAttributes,
  Count,
  SpecificAttributes,
  Select'
  #-}
