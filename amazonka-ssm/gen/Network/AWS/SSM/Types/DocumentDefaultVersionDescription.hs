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
-- Module      : Network.AWS.SSM.Types.DocumentDefaultVersionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentDefaultVersionDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A default version of a document.
--
-- /See:/ 'newDocumentDefaultVersionDescription' smart constructor.
data DocumentDefaultVersionDescription = DocumentDefaultVersionDescription'
  { -- | The default version of the document.
    defaultVersion :: Core.Maybe Core.Text,
    -- | The name of the document.
    name :: Core.Maybe Core.Text,
    -- | The default version of the artifact associated with the document.
    defaultVersionName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentDefaultVersionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVersion', 'documentDefaultVersionDescription_defaultVersion' - The default version of the document.
--
-- 'name', 'documentDefaultVersionDescription_name' - The name of the document.
--
-- 'defaultVersionName', 'documentDefaultVersionDescription_defaultVersionName' - The default version of the artifact associated with the document.
newDocumentDefaultVersionDescription ::
  DocumentDefaultVersionDescription
newDocumentDefaultVersionDescription =
  DocumentDefaultVersionDescription'
    { defaultVersion =
        Core.Nothing,
      name = Core.Nothing,
      defaultVersionName = Core.Nothing
    }

-- | The default version of the document.
documentDefaultVersionDescription_defaultVersion :: Lens.Lens' DocumentDefaultVersionDescription (Core.Maybe Core.Text)
documentDefaultVersionDescription_defaultVersion = Lens.lens (\DocumentDefaultVersionDescription' {defaultVersion} -> defaultVersion) (\s@DocumentDefaultVersionDescription' {} a -> s {defaultVersion = a} :: DocumentDefaultVersionDescription)

-- | The name of the document.
documentDefaultVersionDescription_name :: Lens.Lens' DocumentDefaultVersionDescription (Core.Maybe Core.Text)
documentDefaultVersionDescription_name = Lens.lens (\DocumentDefaultVersionDescription' {name} -> name) (\s@DocumentDefaultVersionDescription' {} a -> s {name = a} :: DocumentDefaultVersionDescription)

-- | The default version of the artifact associated with the document.
documentDefaultVersionDescription_defaultVersionName :: Lens.Lens' DocumentDefaultVersionDescription (Core.Maybe Core.Text)
documentDefaultVersionDescription_defaultVersionName = Lens.lens (\DocumentDefaultVersionDescription' {defaultVersionName} -> defaultVersionName) (\s@DocumentDefaultVersionDescription' {} a -> s {defaultVersionName = a} :: DocumentDefaultVersionDescription)

instance
  Core.FromJSON
    DocumentDefaultVersionDescription
  where
  parseJSON =
    Core.withObject
      "DocumentDefaultVersionDescription"
      ( \x ->
          DocumentDefaultVersionDescription'
            Core.<$> (x Core..:? "DefaultVersion")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "DefaultVersionName")
      )

instance
  Core.Hashable
    DocumentDefaultVersionDescription

instance
  Core.NFData
    DocumentDefaultVersionDescription
