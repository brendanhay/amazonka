{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SAMLIdp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.SAMLIdp
  ( SAMLIdp (..)
  -- * Smart constructor
  , mkSAMLIdp
  -- * Lenses
  , samliMetadataContent
  , samliEntityId
  ) where

import qualified Network.AWS.ElasticSearch.Types.SAMLEntityId as Types
import qualified Network.AWS.ElasticSearch.Types.SAMLMetadata as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the SAML Identity Provider's information.
--
-- /See:/ 'mkSAMLIdp' smart constructor.
data SAMLIdp = SAMLIdp'
  { metadataContent :: Types.SAMLMetadata
    -- ^ The Metadata of the SAML application in xml format.
  , entityId :: Types.SAMLEntityId
    -- ^ The unique Entity ID of the application in SAML Identity Provider.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SAMLIdp' value with any optional fields omitted.
mkSAMLIdp
    :: Types.SAMLMetadata -- ^ 'metadataContent'
    -> Types.SAMLEntityId -- ^ 'entityId'
    -> SAMLIdp
mkSAMLIdp metadataContent entityId
  = SAMLIdp'{metadataContent, entityId}

-- | The Metadata of the SAML application in xml format.
--
-- /Note:/ Consider using 'metadataContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samliMetadataContent :: Lens.Lens' SAMLIdp Types.SAMLMetadata
samliMetadataContent = Lens.field @"metadataContent"
{-# INLINEABLE samliMetadataContent #-}
{-# DEPRECATED metadataContent "Use generic-lens or generic-optics with 'metadataContent' instead"  #-}

-- | The unique Entity ID of the application in SAML Identity Provider.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samliEntityId :: Lens.Lens' SAMLIdp Types.SAMLEntityId
samliEntityId = Lens.field @"entityId"
{-# INLINEABLE samliEntityId #-}
{-# DEPRECATED entityId "Use generic-lens or generic-optics with 'entityId' instead"  #-}

instance Core.FromJSON SAMLIdp where
        toJSON SAMLIdp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MetadataContent" Core..= metadataContent),
                  Core.Just ("EntityId" Core..= entityId)])

instance Core.FromJSON SAMLIdp where
        parseJSON
          = Core.withObject "SAMLIdp" Core.$
              \ x ->
                SAMLIdp' Core.<$>
                  (x Core..: "MetadataContent") Core.<*> x Core..: "EntityId"
