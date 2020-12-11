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
        CommercialItem,
        Date,
        Event,
        Location,
        Organization,
        Other,
        Person,
        Quantity,
        Title
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

pattern CommercialItem :: EntityType
pattern CommercialItem = EntityType' "COMMERCIAL_ITEM"

pattern Date :: EntityType
pattern Date = EntityType' "DATE"

pattern Event :: EntityType
pattern Event = EntityType' "EVENT"

pattern Location :: EntityType
pattern Location = EntityType' "LOCATION"

pattern Organization :: EntityType
pattern Organization = EntityType' "ORGANIZATION"

pattern Other :: EntityType
pattern Other = EntityType' "OTHER"

pattern Person :: EntityType
pattern Person = EntityType' "PERSON"

pattern Quantity :: EntityType
pattern Quantity = EntityType' "QUANTITY"

pattern Title :: EntityType
pattern Title = EntityType' "TITLE"

{-# COMPLETE
  CommercialItem,
  Date,
  Event,
  Location,
  Organization,
  Other,
  Person,
  Quantity,
  Title,
  EntityType'
  #-}
