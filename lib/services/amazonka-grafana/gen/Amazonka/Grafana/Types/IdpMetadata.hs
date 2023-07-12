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
-- Module      : Amazonka.Grafana.Types.IdpMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.IdpMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the identity provider (IdP) metadata used to
-- integrate the identity provider with this workspace. You can specify the
-- metadata either by providing a URL to its location in the @url@
-- parameter, or by specifying the full metadata in XML format in the @xml@
-- parameter. Specifying both will cause an error.
--
-- /See:/ 'newIdpMetadata' smart constructor.
data IdpMetadata = IdpMetadata'
  { -- | The URL of the location containing the IdP metadata.
    url :: Prelude.Maybe Prelude.Text,
    -- | The full IdP metadata, in XML format.
    xml :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdpMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'idpMetadata_url' - The URL of the location containing the IdP metadata.
--
-- 'xml', 'idpMetadata_xml' - The full IdP metadata, in XML format.
newIdpMetadata ::
  IdpMetadata
newIdpMetadata =
  IdpMetadata'
    { url = Prelude.Nothing,
      xml = Prelude.Nothing
    }

-- | The URL of the location containing the IdP metadata.
idpMetadata_url :: Lens.Lens' IdpMetadata (Prelude.Maybe Prelude.Text)
idpMetadata_url = Lens.lens (\IdpMetadata' {url} -> url) (\s@IdpMetadata' {} a -> s {url = a} :: IdpMetadata)

-- | The full IdP metadata, in XML format.
idpMetadata_xml :: Lens.Lens' IdpMetadata (Prelude.Maybe Prelude.Text)
idpMetadata_xml = Lens.lens (\IdpMetadata' {xml} -> xml) (\s@IdpMetadata' {} a -> s {xml = a} :: IdpMetadata)

instance Data.FromJSON IdpMetadata where
  parseJSON =
    Data.withObject
      "IdpMetadata"
      ( \x ->
          IdpMetadata'
            Prelude.<$> (x Data..:? "url")
            Prelude.<*> (x Data..:? "xml")
      )

instance Prelude.Hashable IdpMetadata where
  hashWithSalt _salt IdpMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` xml

instance Prelude.NFData IdpMetadata where
  rnf IdpMetadata' {..} =
    Prelude.rnf url `Prelude.seq` Prelude.rnf xml

instance Data.ToJSON IdpMetadata where
  toJSON IdpMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("url" Data..=) Prelude.<$> url,
            ("xml" Data..=) Prelude.<$> xml
          ]
      )
