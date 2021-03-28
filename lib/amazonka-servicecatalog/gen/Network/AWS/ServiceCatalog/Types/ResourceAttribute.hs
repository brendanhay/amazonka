{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ResourceAttribute
  ( ResourceAttribute
    ( ResourceAttribute'
    , ResourceAttributeProperties
    , ResourceAttributeMetadata
    , ResourceAttributeCreationpolicy
    , ResourceAttributeUpdatepolicy
    , ResourceAttributeDeletionpolicy
    , ResourceAttributeTags
    , fromResourceAttribute
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ResourceAttribute = ResourceAttribute'{fromResourceAttribute
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern ResourceAttributeProperties :: ResourceAttribute
pattern ResourceAttributeProperties = ResourceAttribute' "PROPERTIES"

pattern ResourceAttributeMetadata :: ResourceAttribute
pattern ResourceAttributeMetadata = ResourceAttribute' "METADATA"

pattern ResourceAttributeCreationpolicy :: ResourceAttribute
pattern ResourceAttributeCreationpolicy = ResourceAttribute' "CREATIONPOLICY"

pattern ResourceAttributeUpdatepolicy :: ResourceAttribute
pattern ResourceAttributeUpdatepolicy = ResourceAttribute' "UPDATEPOLICY"

pattern ResourceAttributeDeletionpolicy :: ResourceAttribute
pattern ResourceAttributeDeletionpolicy = ResourceAttribute' "DELETIONPOLICY"

pattern ResourceAttributeTags :: ResourceAttribute
pattern ResourceAttributeTags = ResourceAttribute' "TAGS"

{-# COMPLETE 
  ResourceAttributeProperties,

  ResourceAttributeMetadata,

  ResourceAttributeCreationpolicy,

  ResourceAttributeUpdatepolicy,

  ResourceAttributeDeletionpolicy,

  ResourceAttributeTags,
  ResourceAttribute'
  #-}
