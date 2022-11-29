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
-- Module      : Amazonka.AppFlow.Types.VeevaSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.VeevaSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when using Veeva as a flow source.
--
-- /See:/ 'newVeevaSourceProperties' smart constructor.
data VeevaSourceProperties = VeevaSourceProperties'
  { -- | The document type specified in the Veeva document extract flow.
    documentType :: Prelude.Maybe Prelude.Text,
    -- | Boolean value to include source files in Veeva document extract flow.
    includeSourceFiles :: Prelude.Maybe Prelude.Bool,
    -- | Boolean value to include file renditions in Veeva document extract flow.
    includeRenditions :: Prelude.Maybe Prelude.Bool,
    -- | Boolean value to include All Versions of files in Veeva document extract
    -- flow.
    includeAllVersions :: Prelude.Maybe Prelude.Bool,
    -- | The object specified in the Veeva flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VeevaSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentType', 'veevaSourceProperties_documentType' - The document type specified in the Veeva document extract flow.
--
-- 'includeSourceFiles', 'veevaSourceProperties_includeSourceFiles' - Boolean value to include source files in Veeva document extract flow.
--
-- 'includeRenditions', 'veevaSourceProperties_includeRenditions' - Boolean value to include file renditions in Veeva document extract flow.
--
-- 'includeAllVersions', 'veevaSourceProperties_includeAllVersions' - Boolean value to include All Versions of files in Veeva document extract
-- flow.
--
-- 'object'', 'veevaSourceProperties_object' - The object specified in the Veeva flow source.
newVeevaSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  VeevaSourceProperties
newVeevaSourceProperties pObject_ =
  VeevaSourceProperties'
    { documentType =
        Prelude.Nothing,
      includeSourceFiles = Prelude.Nothing,
      includeRenditions = Prelude.Nothing,
      includeAllVersions = Prelude.Nothing,
      object' = pObject_
    }

-- | The document type specified in the Veeva document extract flow.
veevaSourceProperties_documentType :: Lens.Lens' VeevaSourceProperties (Prelude.Maybe Prelude.Text)
veevaSourceProperties_documentType = Lens.lens (\VeevaSourceProperties' {documentType} -> documentType) (\s@VeevaSourceProperties' {} a -> s {documentType = a} :: VeevaSourceProperties)

-- | Boolean value to include source files in Veeva document extract flow.
veevaSourceProperties_includeSourceFiles :: Lens.Lens' VeevaSourceProperties (Prelude.Maybe Prelude.Bool)
veevaSourceProperties_includeSourceFiles = Lens.lens (\VeevaSourceProperties' {includeSourceFiles} -> includeSourceFiles) (\s@VeevaSourceProperties' {} a -> s {includeSourceFiles = a} :: VeevaSourceProperties)

-- | Boolean value to include file renditions in Veeva document extract flow.
veevaSourceProperties_includeRenditions :: Lens.Lens' VeevaSourceProperties (Prelude.Maybe Prelude.Bool)
veevaSourceProperties_includeRenditions = Lens.lens (\VeevaSourceProperties' {includeRenditions} -> includeRenditions) (\s@VeevaSourceProperties' {} a -> s {includeRenditions = a} :: VeevaSourceProperties)

-- | Boolean value to include All Versions of files in Veeva document extract
-- flow.
veevaSourceProperties_includeAllVersions :: Lens.Lens' VeevaSourceProperties (Prelude.Maybe Prelude.Bool)
veevaSourceProperties_includeAllVersions = Lens.lens (\VeevaSourceProperties' {includeAllVersions} -> includeAllVersions) (\s@VeevaSourceProperties' {} a -> s {includeAllVersions = a} :: VeevaSourceProperties)

-- | The object specified in the Veeva flow source.
veevaSourceProperties_object :: Lens.Lens' VeevaSourceProperties Prelude.Text
veevaSourceProperties_object = Lens.lens (\VeevaSourceProperties' {object'} -> object') (\s@VeevaSourceProperties' {} a -> s {object' = a} :: VeevaSourceProperties)

instance Core.FromJSON VeevaSourceProperties where
  parseJSON =
    Core.withObject
      "VeevaSourceProperties"
      ( \x ->
          VeevaSourceProperties'
            Prelude.<$> (x Core..:? "documentType")
            Prelude.<*> (x Core..:? "includeSourceFiles")
            Prelude.<*> (x Core..:? "includeRenditions")
            Prelude.<*> (x Core..:? "includeAllVersions")
            Prelude.<*> (x Core..: "object")
      )

instance Prelude.Hashable VeevaSourceProperties where
  hashWithSalt _salt VeevaSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` documentType
      `Prelude.hashWithSalt` includeSourceFiles
      `Prelude.hashWithSalt` includeRenditions
      `Prelude.hashWithSalt` includeAllVersions
      `Prelude.hashWithSalt` object'

instance Prelude.NFData VeevaSourceProperties where
  rnf VeevaSourceProperties' {..} =
    Prelude.rnf documentType
      `Prelude.seq` Prelude.rnf includeSourceFiles
      `Prelude.seq` Prelude.rnf includeRenditions
      `Prelude.seq` Prelude.rnf includeAllVersions
      `Prelude.seq` Prelude.rnf object'

instance Core.ToJSON VeevaSourceProperties where
  toJSON VeevaSourceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("documentType" Core..=) Prelude.<$> documentType,
            ("includeSourceFiles" Core..=)
              Prelude.<$> includeSourceFiles,
            ("includeRenditions" Core..=)
              Prelude.<$> includeRenditions,
            ("includeAllVersions" Core..=)
              Prelude.<$> includeAllVersions,
            Prelude.Just ("object" Core..= object')
          ]
      )
