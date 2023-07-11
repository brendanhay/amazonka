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
-- Module      : Amazonka.APIGateway.Types.DocumentationVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.DocumentationVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A snapshot of the documentation of an API.
--
-- /See:/ 'newDocumentationVersion' smart constructor.
data DocumentationVersion = DocumentationVersion'
  { -- | The date when the API documentation snapshot is created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the API documentation snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The version identifier of the API documentation snapshot.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'documentationVersion_createdDate' - The date when the API documentation snapshot is created.
--
-- 'description', 'documentationVersion_description' - The description of the API documentation snapshot.
--
-- 'version', 'documentationVersion_version' - The version identifier of the API documentation snapshot.
newDocumentationVersion ::
  DocumentationVersion
newDocumentationVersion =
  DocumentationVersion'
    { createdDate =
        Prelude.Nothing,
      description = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The date when the API documentation snapshot is created.
documentationVersion_createdDate :: Lens.Lens' DocumentationVersion (Prelude.Maybe Prelude.UTCTime)
documentationVersion_createdDate = Lens.lens (\DocumentationVersion' {createdDate} -> createdDate) (\s@DocumentationVersion' {} a -> s {createdDate = a} :: DocumentationVersion) Prelude.. Lens.mapping Data._Time

-- | The description of the API documentation snapshot.
documentationVersion_description :: Lens.Lens' DocumentationVersion (Prelude.Maybe Prelude.Text)
documentationVersion_description = Lens.lens (\DocumentationVersion' {description} -> description) (\s@DocumentationVersion' {} a -> s {description = a} :: DocumentationVersion)

-- | The version identifier of the API documentation snapshot.
documentationVersion_version :: Lens.Lens' DocumentationVersion (Prelude.Maybe Prelude.Text)
documentationVersion_version = Lens.lens (\DocumentationVersion' {version} -> version) (\s@DocumentationVersion' {} a -> s {version = a} :: DocumentationVersion)

instance Data.FromJSON DocumentationVersion where
  parseJSON =
    Data.withObject
      "DocumentationVersion"
      ( \x ->
          DocumentationVersion'
            Prelude.<$> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable DocumentationVersion where
  hashWithSalt _salt DocumentationVersion' {..} =
    _salt
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` version

instance Prelude.NFData DocumentationVersion where
  rnf DocumentationVersion' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf version
