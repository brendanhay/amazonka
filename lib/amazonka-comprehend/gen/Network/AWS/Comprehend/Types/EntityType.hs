{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityType
  ( EntityType
      ( EntityType',
        Person,
        Location,
        Organization,
        CommercialItem,
        Event,
        Date,
        Quantity,
        Title,
        Other
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EntityType = EntityType' Lude.Text
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

pattern Person :: EntityType
pattern Person = EntityType' "PERSON"

pattern Location :: EntityType
pattern Location = EntityType' "LOCATION"

pattern Organization :: EntityType
pattern Organization = EntityType' "ORGANIZATION"

pattern CommercialItem :: EntityType
pattern CommercialItem = EntityType' "COMMERCIAL_ITEM"

pattern Event :: EntityType
pattern Event = EntityType' "EVENT"

pattern Date :: EntityType
pattern Date = EntityType' "DATE"

pattern Quantity :: EntityType
pattern Quantity = EntityType' "QUANTITY"

pattern Title :: EntityType
pattern Title = EntityType' "TITLE"

pattern Other :: EntityType
pattern Other = EntityType' "OTHER"

{-# COMPLETE
  Person,
  Location,
  Organization,
  CommercialItem,
  Event,
  Date,
  Quantity,
  Title,
  Other,
  EntityType'
  #-}
