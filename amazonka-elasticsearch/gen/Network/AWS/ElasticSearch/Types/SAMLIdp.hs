{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SAMLIdp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SAMLIdp where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the SAML Identity Provider\'s information.
--
-- /See:/ 'newSAMLIdp' smart constructor.
data SAMLIdp = SAMLIdp'
  { -- | The Metadata of the SAML application in xml format.
    metadataContent :: Core.Text,
    -- | The unique Entity ID of the application in SAML Identity Provider.
    entityId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SAMLIdp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataContent', 'sAMLIdp_metadataContent' - The Metadata of the SAML application in xml format.
--
-- 'entityId', 'sAMLIdp_entityId' - The unique Entity ID of the application in SAML Identity Provider.
newSAMLIdp ::
  -- | 'metadataContent'
  Core.Text ->
  -- | 'entityId'
  Core.Text ->
  SAMLIdp
newSAMLIdp pMetadataContent_ pEntityId_ =
  SAMLIdp'
    { metadataContent = pMetadataContent_,
      entityId = pEntityId_
    }

-- | The Metadata of the SAML application in xml format.
sAMLIdp_metadataContent :: Lens.Lens' SAMLIdp Core.Text
sAMLIdp_metadataContent = Lens.lens (\SAMLIdp' {metadataContent} -> metadataContent) (\s@SAMLIdp' {} a -> s {metadataContent = a} :: SAMLIdp)

-- | The unique Entity ID of the application in SAML Identity Provider.
sAMLIdp_entityId :: Lens.Lens' SAMLIdp Core.Text
sAMLIdp_entityId = Lens.lens (\SAMLIdp' {entityId} -> entityId) (\s@SAMLIdp' {} a -> s {entityId = a} :: SAMLIdp)

instance Core.FromJSON SAMLIdp where
  parseJSON =
    Core.withObject
      "SAMLIdp"
      ( \x ->
          SAMLIdp'
            Core.<$> (x Core..: "MetadataContent")
            Core.<*> (x Core..: "EntityId")
      )

instance Core.Hashable SAMLIdp

instance Core.NFData SAMLIdp

instance Core.ToJSON SAMLIdp where
  toJSON SAMLIdp' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("MetadataContent" Core..= metadataContent),
            Core.Just ("EntityId" Core..= entityId)
          ]
      )
