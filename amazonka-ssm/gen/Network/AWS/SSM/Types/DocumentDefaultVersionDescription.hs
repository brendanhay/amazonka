{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A default version of a document.
--
-- /See:/ 'newDocumentDefaultVersionDescription' smart constructor.
data DocumentDefaultVersionDescription = DocumentDefaultVersionDescription'
  { -- | The default version of the document.
    defaultVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The default version of the artifact associated with the document.
    defaultVersionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      name = Prelude.Nothing,
      defaultVersionName = Prelude.Nothing
    }

-- | The default version of the document.
documentDefaultVersionDescription_defaultVersion :: Lens.Lens' DocumentDefaultVersionDescription (Prelude.Maybe Prelude.Text)
documentDefaultVersionDescription_defaultVersion = Lens.lens (\DocumentDefaultVersionDescription' {defaultVersion} -> defaultVersion) (\s@DocumentDefaultVersionDescription' {} a -> s {defaultVersion = a} :: DocumentDefaultVersionDescription)

-- | The name of the document.
documentDefaultVersionDescription_name :: Lens.Lens' DocumentDefaultVersionDescription (Prelude.Maybe Prelude.Text)
documentDefaultVersionDescription_name = Lens.lens (\DocumentDefaultVersionDescription' {name} -> name) (\s@DocumentDefaultVersionDescription' {} a -> s {name = a} :: DocumentDefaultVersionDescription)

-- | The default version of the artifact associated with the document.
documentDefaultVersionDescription_defaultVersionName :: Lens.Lens' DocumentDefaultVersionDescription (Prelude.Maybe Prelude.Text)
documentDefaultVersionDescription_defaultVersionName = Lens.lens (\DocumentDefaultVersionDescription' {defaultVersionName} -> defaultVersionName) (\s@DocumentDefaultVersionDescription' {} a -> s {defaultVersionName = a} :: DocumentDefaultVersionDescription)

instance
  Prelude.FromJSON
    DocumentDefaultVersionDescription
  where
  parseJSON =
    Prelude.withObject
      "DocumentDefaultVersionDescription"
      ( \x ->
          DocumentDefaultVersionDescription'
            Prelude.<$> (x Prelude..:? "DefaultVersion")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "DefaultVersionName")
      )

instance
  Prelude.Hashable
    DocumentDefaultVersionDescription

instance
  Prelude.NFData
    DocumentDefaultVersionDescription
