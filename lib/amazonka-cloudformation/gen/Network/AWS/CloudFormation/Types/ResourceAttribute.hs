{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ResourceAttribute
  ( ResourceAttribute
    ( ResourceAttribute'
    , ResourceAttributeProperties
    , ResourceAttributeMetadata
    , ResourceAttributeCreationPolicy
    , ResourceAttributeUpdatePolicy
    , ResourceAttributeDeletionPolicy
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
pattern ResourceAttributeProperties = ResourceAttribute' "Properties"

pattern ResourceAttributeMetadata :: ResourceAttribute
pattern ResourceAttributeMetadata = ResourceAttribute' "Metadata"

pattern ResourceAttributeCreationPolicy :: ResourceAttribute
pattern ResourceAttributeCreationPolicy = ResourceAttribute' "CreationPolicy"

pattern ResourceAttributeUpdatePolicy :: ResourceAttribute
pattern ResourceAttributeUpdatePolicy = ResourceAttribute' "UpdatePolicy"

pattern ResourceAttributeDeletionPolicy :: ResourceAttribute
pattern ResourceAttributeDeletionPolicy = ResourceAttribute' "DeletionPolicy"

pattern ResourceAttributeTags :: ResourceAttribute
pattern ResourceAttributeTags = ResourceAttribute' "Tags"

{-# COMPLETE 
  ResourceAttributeProperties,

  ResourceAttributeMetadata,

  ResourceAttributeCreationPolicy,

  ResourceAttributeUpdatePolicy,

  ResourceAttributeDeletionPolicy,

  ResourceAttributeTags,
  ResourceAttribute'
  #-}
