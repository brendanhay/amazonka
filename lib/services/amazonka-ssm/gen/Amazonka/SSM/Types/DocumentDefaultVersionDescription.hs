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
-- Module      : Amazonka.SSM.Types.DocumentDefaultVersionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentDefaultVersionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A default version of a document.
--
-- /See:/ 'newDocumentDefaultVersionDescription' smart constructor.
data DocumentDefaultVersionDescription = DocumentDefaultVersionDescription'
  { -- | The default version of the artifact associated with the document.
    defaultVersionName :: Prelude.Maybe Prelude.Text,
    -- | The default version of the document.
    defaultVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the document.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentDefaultVersionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVersionName', 'documentDefaultVersionDescription_defaultVersionName' - The default version of the artifact associated with the document.
--
-- 'defaultVersion', 'documentDefaultVersionDescription_defaultVersion' - The default version of the document.
--
-- 'name', 'documentDefaultVersionDescription_name' - The name of the document.
newDocumentDefaultVersionDescription ::
  DocumentDefaultVersionDescription
newDocumentDefaultVersionDescription =
  DocumentDefaultVersionDescription'
    { defaultVersionName =
        Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The default version of the artifact associated with the document.
documentDefaultVersionDescription_defaultVersionName :: Lens.Lens' DocumentDefaultVersionDescription (Prelude.Maybe Prelude.Text)
documentDefaultVersionDescription_defaultVersionName = Lens.lens (\DocumentDefaultVersionDescription' {defaultVersionName} -> defaultVersionName) (\s@DocumentDefaultVersionDescription' {} a -> s {defaultVersionName = a} :: DocumentDefaultVersionDescription)

-- | The default version of the document.
documentDefaultVersionDescription_defaultVersion :: Lens.Lens' DocumentDefaultVersionDescription (Prelude.Maybe Prelude.Text)
documentDefaultVersionDescription_defaultVersion = Lens.lens (\DocumentDefaultVersionDescription' {defaultVersion} -> defaultVersion) (\s@DocumentDefaultVersionDescription' {} a -> s {defaultVersion = a} :: DocumentDefaultVersionDescription)

-- | The name of the document.
documentDefaultVersionDescription_name :: Lens.Lens' DocumentDefaultVersionDescription (Prelude.Maybe Prelude.Text)
documentDefaultVersionDescription_name = Lens.lens (\DocumentDefaultVersionDescription' {name} -> name) (\s@DocumentDefaultVersionDescription' {} a -> s {name = a} :: DocumentDefaultVersionDescription)

instance
  Core.FromJSON
    DocumentDefaultVersionDescription
  where
  parseJSON =
    Core.withObject
      "DocumentDefaultVersionDescription"
      ( \x ->
          DocumentDefaultVersionDescription'
            Prelude.<$> (x Core..:? "DefaultVersionName")
            Prelude.<*> (x Core..:? "DefaultVersion")
            Prelude.<*> (x Core..:? "Name")
      )

instance
  Prelude.Hashable
    DocumentDefaultVersionDescription

instance
  Prelude.NFData
    DocumentDefaultVersionDescription
