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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.DocumentationVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A snapshot of the documentation of an API.
--
-- Publishing API documentation involves creating a documentation version
-- associated with an API stage and exporting the versioned documentation
-- to an external (e.g., OpenAPI) file.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API>,
-- DocumentationPart, DocumentationVersions
--
-- /See:/ 'newDocumentationVersion' smart constructor.
data DocumentationVersion = DocumentationVersion'
  { -- | The date when the API documentation snapshot is created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The version identifier of the API documentation snapshot.
    version :: Prelude.Maybe Prelude.Text,
    -- | The description of the API documentation snapshot.
    description :: Prelude.Maybe Prelude.Text
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
-- 'version', 'documentationVersion_version' - The version identifier of the API documentation snapshot.
--
-- 'description', 'documentationVersion_description' - The description of the API documentation snapshot.
newDocumentationVersion ::
  DocumentationVersion
newDocumentationVersion =
  DocumentationVersion'
    { createdDate =
        Prelude.Nothing,
      version = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The date when the API documentation snapshot is created.
documentationVersion_createdDate :: Lens.Lens' DocumentationVersion (Prelude.Maybe Prelude.UTCTime)
documentationVersion_createdDate = Lens.lens (\DocumentationVersion' {createdDate} -> createdDate) (\s@DocumentationVersion' {} a -> s {createdDate = a} :: DocumentationVersion) Prelude.. Lens.mapping Core._Time

-- | The version identifier of the API documentation snapshot.
documentationVersion_version :: Lens.Lens' DocumentationVersion (Prelude.Maybe Prelude.Text)
documentationVersion_version = Lens.lens (\DocumentationVersion' {version} -> version) (\s@DocumentationVersion' {} a -> s {version = a} :: DocumentationVersion)

-- | The description of the API documentation snapshot.
documentationVersion_description :: Lens.Lens' DocumentationVersion (Prelude.Maybe Prelude.Text)
documentationVersion_description = Lens.lens (\DocumentationVersion' {description} -> description) (\s@DocumentationVersion' {} a -> s {description = a} :: DocumentationVersion)

instance Core.FromJSON DocumentationVersion where
  parseJSON =
    Core.withObject
      "DocumentationVersion"
      ( \x ->
          DocumentationVersion'
            Prelude.<$> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable DocumentationVersion where
  hashWithSalt _salt DocumentationVersion' {..} =
    _salt `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` description

instance Prelude.NFData DocumentationVersion where
  rnf DocumentationVersion' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf description
