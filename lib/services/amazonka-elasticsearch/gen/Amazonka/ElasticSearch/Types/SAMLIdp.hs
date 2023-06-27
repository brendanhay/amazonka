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
-- Module      : Amazonka.ElasticSearch.Types.SAMLIdp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.SAMLIdp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the SAML Identity Provider\'s information.
--
-- /See:/ 'newSAMLIdp' smart constructor.
data SAMLIdp = SAMLIdp'
  { -- | The Metadata of the SAML application in xml format.
    metadataContent :: Prelude.Text,
    -- | The unique Entity ID of the application in SAML Identity Provider.
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
-- 'metadataContent', 'sAMLIdp_metadataContent' - The Metadata of the SAML application in xml format.
--
-- 'entityId', 'sAMLIdp_entityId' - The unique Entity ID of the application in SAML Identity Provider.
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

-- | The Metadata of the SAML application in xml format.
sAMLIdp_metadataContent :: Lens.Lens' SAMLIdp Prelude.Text
sAMLIdp_metadataContent = Lens.lens (\SAMLIdp' {metadataContent} -> metadataContent) (\s@SAMLIdp' {} a -> s {metadataContent = a} :: SAMLIdp)

-- | The unique Entity ID of the application in SAML Identity Provider.
sAMLIdp_entityId :: Lens.Lens' SAMLIdp Prelude.Text
sAMLIdp_entityId = Lens.lens (\SAMLIdp' {entityId} -> entityId) (\s@SAMLIdp' {} a -> s {entityId = a} :: SAMLIdp)

instance Data.FromJSON SAMLIdp where
  parseJSON =
    Data.withObject
      "SAMLIdp"
      ( \x ->
          SAMLIdp'
            Prelude.<$> (x Data..: "MetadataContent")
            Prelude.<*> (x Data..: "EntityId")
      )

instance Prelude.Hashable SAMLIdp where
  hashWithSalt _salt SAMLIdp' {..} =
    _salt
      `Prelude.hashWithSalt` metadataContent
      `Prelude.hashWithSalt` entityId

instance Prelude.NFData SAMLIdp where
  rnf SAMLIdp' {..} =
    Prelude.rnf metadataContent
      `Prelude.seq` Prelude.rnf entityId

instance Data.ToJSON SAMLIdp where
  toJSON SAMLIdp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MetadataContent" Data..= metadataContent),
            Prelude.Just ("EntityId" Data..= entityId)
          ]
      )
