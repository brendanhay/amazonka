{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SAMLIdp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SAMLIdp
  ( SAMLIdp (..),

    -- * Smart constructor
    mkSAMLIdp,

    -- * Lenses
    samliMetadataContent,
    samliEntityId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the SAML Identity Provider's information.
--
-- /See:/ 'mkSAMLIdp' smart constructor.
data SAMLIdp = SAMLIdp'
  { metadataContent :: Lude.Text,
    entityId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SAMLIdp' with the minimum fields required to make a request.
--
-- * 'entityId' - The unique Entity ID of the application in SAML Identity Provider.
-- * 'metadataContent' - The Metadata of the SAML application in xml format.
mkSAMLIdp ::
  -- | 'metadataContent'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  SAMLIdp
mkSAMLIdp pMetadataContent_ pEntityId_ =
  SAMLIdp'
    { metadataContent = pMetadataContent_,
      entityId = pEntityId_
    }

-- | The Metadata of the SAML application in xml format.
--
-- /Note:/ Consider using 'metadataContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samliMetadataContent :: Lens.Lens' SAMLIdp Lude.Text
samliMetadataContent = Lens.lens (metadataContent :: SAMLIdp -> Lude.Text) (\s a -> s {metadataContent = a} :: SAMLIdp)
{-# DEPRECATED samliMetadataContent "Use generic-lens or generic-optics with 'metadataContent' instead." #-}

-- | The unique Entity ID of the application in SAML Identity Provider.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samliEntityId :: Lens.Lens' SAMLIdp Lude.Text
samliEntityId = Lens.lens (entityId :: SAMLIdp -> Lude.Text) (\s a -> s {entityId = a} :: SAMLIdp)
{-# DEPRECATED samliEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

instance Lude.FromJSON SAMLIdp where
  parseJSON =
    Lude.withObject
      "SAMLIdp"
      ( \x ->
          SAMLIdp'
            Lude.<$> (x Lude..: "MetadataContent") Lude.<*> (x Lude..: "EntityId")
      )

instance Lude.ToJSON SAMLIdp where
  toJSON SAMLIdp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MetadataContent" Lude..= metadataContent),
            Lude.Just ("EntityId" Lude..= entityId)
          ]
      )
