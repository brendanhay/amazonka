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
-- Module      : Network.AWS.OpenSearch.Types.SAMLIdp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.SAMLIdp where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The SAML identity povider\'s information.
--
-- /See:/ 'newSAMLIdp' smart constructor.
data SAMLIdp = SAMLIdp'
  { -- | The metadata of the SAML application in XML format.
    metadataContent :: Prelude.Text,
    -- | The unique entity ID of the application in SAML identity provider.
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SAMLIdp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataContent', 'sAMLIdp_metadataContent' - The metadata of the SAML application in XML format.
--
-- 'entityId', 'sAMLIdp_entityId' - The unique entity ID of the application in SAML identity provider.
newSAMLIdp ::
  -- | 'metadataContent'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  SAMLIdp
newSAMLIdp pMetadataContent_ pEntityId_ =
  SAMLIdp'
    { metadataContent = pMetadataContent_,
      entityId = pEntityId_
    }

-- | The metadata of the SAML application in XML format.
sAMLIdp_metadataContent :: Lens.Lens' SAMLIdp Prelude.Text
sAMLIdp_metadataContent = Lens.lens (\SAMLIdp' {metadataContent} -> metadataContent) (\s@SAMLIdp' {} a -> s {metadataContent = a} :: SAMLIdp)

-- | The unique entity ID of the application in SAML identity provider.
sAMLIdp_entityId :: Lens.Lens' SAMLIdp Prelude.Text
sAMLIdp_entityId = Lens.lens (\SAMLIdp' {entityId} -> entityId) (\s@SAMLIdp' {} a -> s {entityId = a} :: SAMLIdp)

instance Core.FromJSON SAMLIdp where
  parseJSON =
    Core.withObject
      "SAMLIdp"
      ( \x ->
          SAMLIdp'
            Prelude.<$> (x Core..: "MetadataContent")
            Prelude.<*> (x Core..: "EntityId")
      )

instance Prelude.Hashable SAMLIdp

instance Prelude.NFData SAMLIdp

instance Core.ToJSON SAMLIdp where
  toJSON SAMLIdp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MetadataContent" Core..= metadataContent),
            Prelude.Just ("EntityId" Core..= entityId)
          ]
      )
