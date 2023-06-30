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
-- Module      : Amazonka.GamesParks.Types.ExtensionVersionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.ExtensionVersionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the extension version.
--
-- /See:/ 'newExtensionVersionDetails' smart constructor.
data ExtensionVersionDetails = ExtensionVersionDetails'
  { -- | The name of the extension.
    name :: Prelude.Maybe Prelude.Text,
    -- | The namespace (qualifier) of the extension.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The model that defines the interface for this extension version.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The version of the extension.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtensionVersionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'extensionVersionDetails_name' - The name of the extension.
--
-- 'namespace', 'extensionVersionDetails_namespace' - The namespace (qualifier) of the extension.
--
-- 'schema', 'extensionVersionDetails_schema' - The model that defines the interface for this extension version.
--
-- 'version', 'extensionVersionDetails_version' - The version of the extension.
newExtensionVersionDetails ::
  ExtensionVersionDetails
newExtensionVersionDetails =
  ExtensionVersionDetails'
    { name = Prelude.Nothing,
      namespace = Prelude.Nothing,
      schema = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the extension.
extensionVersionDetails_name :: Lens.Lens' ExtensionVersionDetails (Prelude.Maybe Prelude.Text)
extensionVersionDetails_name = Lens.lens (\ExtensionVersionDetails' {name} -> name) (\s@ExtensionVersionDetails' {} a -> s {name = a} :: ExtensionVersionDetails)

-- | The namespace (qualifier) of the extension.
extensionVersionDetails_namespace :: Lens.Lens' ExtensionVersionDetails (Prelude.Maybe Prelude.Text)
extensionVersionDetails_namespace = Lens.lens (\ExtensionVersionDetails' {namespace} -> namespace) (\s@ExtensionVersionDetails' {} a -> s {namespace = a} :: ExtensionVersionDetails)

-- | The model that defines the interface for this extension version.
extensionVersionDetails_schema :: Lens.Lens' ExtensionVersionDetails (Prelude.Maybe Prelude.Text)
extensionVersionDetails_schema = Lens.lens (\ExtensionVersionDetails' {schema} -> schema) (\s@ExtensionVersionDetails' {} a -> s {schema = a} :: ExtensionVersionDetails)

-- | The version of the extension.
extensionVersionDetails_version :: Lens.Lens' ExtensionVersionDetails (Prelude.Maybe Prelude.Text)
extensionVersionDetails_version = Lens.lens (\ExtensionVersionDetails' {version} -> version) (\s@ExtensionVersionDetails' {} a -> s {version = a} :: ExtensionVersionDetails)

instance Data.FromJSON ExtensionVersionDetails where
  parseJSON =
    Data.withObject
      "ExtensionVersionDetails"
      ( \x ->
          ExtensionVersionDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Namespace")
            Prelude.<*> (x Data..:? "Schema")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable ExtensionVersionDetails where
  hashWithSalt _salt ExtensionVersionDetails' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` version

instance Prelude.NFData ExtensionVersionDetails where
  rnf ExtensionVersionDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf version
