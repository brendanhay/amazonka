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
-- Module      : Amazonka.GamesParks.Types.ExtensionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.ExtensionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the extension.
--
-- /See:/ 'newExtensionDetails' smart constructor.
data ExtensionDetails = ExtensionDetails'
  { -- | The description of the extension.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the extension.
    name :: Prelude.Maybe Prelude.Text,
    -- | The namespace (qualifier) of the extension.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtensionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'extensionDetails_description' - The description of the extension.
--
-- 'name', 'extensionDetails_name' - The name of the extension.
--
-- 'namespace', 'extensionDetails_namespace' - The namespace (qualifier) of the extension.
newExtensionDetails ::
  ExtensionDetails
newExtensionDetails =
  ExtensionDetails'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | The description of the extension.
extensionDetails_description :: Lens.Lens' ExtensionDetails (Prelude.Maybe Prelude.Text)
extensionDetails_description = Lens.lens (\ExtensionDetails' {description} -> description) (\s@ExtensionDetails' {} a -> s {description = a} :: ExtensionDetails)

-- | The name of the extension.
extensionDetails_name :: Lens.Lens' ExtensionDetails (Prelude.Maybe Prelude.Text)
extensionDetails_name = Lens.lens (\ExtensionDetails' {name} -> name) (\s@ExtensionDetails' {} a -> s {name = a} :: ExtensionDetails)

-- | The namespace (qualifier) of the extension.
extensionDetails_namespace :: Lens.Lens' ExtensionDetails (Prelude.Maybe Prelude.Text)
extensionDetails_namespace = Lens.lens (\ExtensionDetails' {namespace} -> namespace) (\s@ExtensionDetails' {} a -> s {namespace = a} :: ExtensionDetails)

instance Data.FromJSON ExtensionDetails where
  parseJSON =
    Data.withObject
      "ExtensionDetails"
      ( \x ->
          ExtensionDetails'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Namespace")
      )

instance Prelude.Hashable ExtensionDetails where
  hashWithSalt _salt ExtensionDetails' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData ExtensionDetails where
  rnf ExtensionDetails' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespace
