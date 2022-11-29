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
-- Module      : Amazonka.OpenSearch.Types.SAMLIdp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.SAMLIdp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The SAML identity povider information.
--
-- /See:/ 'newSAMLIdp' smart constructor.
data SAMLIdp = SAMLIdp'
  { -- | The metadata of the SAML application, in XML format.
    metadataContent :: Prelude.Text,
    -- | The unique entity ID of the application in the SAML identity provider.
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
-- 'metadataContent', 'sAMLIdp_metadataContent' - The metadata of the SAML application, in XML format.
--
-- 'entityId', 'sAMLIdp_entityId' - The unique entity ID of the application in the SAML identity provider.
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

-- | The metadata of the SAML application, in XML format.
sAMLIdp_metadataContent :: Lens.Lens' SAMLIdp Prelude.Text
sAMLIdp_metadataContent = Lens.lens (\SAMLIdp' {metadataContent} -> metadataContent) (\s@SAMLIdp' {} a -> s {metadataContent = a} :: SAMLIdp)

-- | The unique entity ID of the application in the SAML identity provider.
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

instance Prelude.Hashable SAMLIdp where
  hashWithSalt _salt SAMLIdp' {..} =
    _salt `Prelude.hashWithSalt` metadataContent
      `Prelude.hashWithSalt` entityId

instance Prelude.NFData SAMLIdp where
  rnf SAMLIdp' {..} =
    Prelude.rnf metadataContent
      `Prelude.seq` Prelude.rnf entityId

instance Core.ToJSON SAMLIdp where
  toJSON SAMLIdp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MetadataContent" Core..= metadataContent),
            Prelude.Just ("EntityId" Core..= entityId)
          ]
      )
