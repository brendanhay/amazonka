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
-- Module      : Amazonka.AppConfig.Types.AppliedExtension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.AppliedExtension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An extension that was invoked during a deployment.
--
-- /See:/ 'newAppliedExtension' smart constructor.
data AppliedExtension = AppliedExtension'
  { -- | The system-generated ID for the association.
    extensionAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the extension.
    extensionId :: Prelude.Maybe Prelude.Text,
    -- | One or more parameters for the actions called by the extension.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The extension version number.
    versionNumber :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppliedExtension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extensionAssociationId', 'appliedExtension_extensionAssociationId' - The system-generated ID for the association.
--
-- 'extensionId', 'appliedExtension_extensionId' - The system-generated ID of the extension.
--
-- 'parameters', 'appliedExtension_parameters' - One or more parameters for the actions called by the extension.
--
-- 'versionNumber', 'appliedExtension_versionNumber' - The extension version number.
newAppliedExtension ::
  AppliedExtension
newAppliedExtension =
  AppliedExtension'
    { extensionAssociationId =
        Prelude.Nothing,
      extensionId = Prelude.Nothing,
      parameters = Prelude.Nothing,
      versionNumber = Prelude.Nothing
    }

-- | The system-generated ID for the association.
appliedExtension_extensionAssociationId :: Lens.Lens' AppliedExtension (Prelude.Maybe Prelude.Text)
appliedExtension_extensionAssociationId = Lens.lens (\AppliedExtension' {extensionAssociationId} -> extensionAssociationId) (\s@AppliedExtension' {} a -> s {extensionAssociationId = a} :: AppliedExtension)

-- | The system-generated ID of the extension.
appliedExtension_extensionId :: Lens.Lens' AppliedExtension (Prelude.Maybe Prelude.Text)
appliedExtension_extensionId = Lens.lens (\AppliedExtension' {extensionId} -> extensionId) (\s@AppliedExtension' {} a -> s {extensionId = a} :: AppliedExtension)

-- | One or more parameters for the actions called by the extension.
appliedExtension_parameters :: Lens.Lens' AppliedExtension (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
appliedExtension_parameters = Lens.lens (\AppliedExtension' {parameters} -> parameters) (\s@AppliedExtension' {} a -> s {parameters = a} :: AppliedExtension) Prelude.. Lens.mapping Lens.coerced

-- | The extension version number.
appliedExtension_versionNumber :: Lens.Lens' AppliedExtension (Prelude.Maybe Prelude.Int)
appliedExtension_versionNumber = Lens.lens (\AppliedExtension' {versionNumber} -> versionNumber) (\s@AppliedExtension' {} a -> s {versionNumber = a} :: AppliedExtension)

instance Data.FromJSON AppliedExtension where
  parseJSON =
    Data.withObject
      "AppliedExtension"
      ( \x ->
          AppliedExtension'
            Prelude.<$> (x Data..:? "ExtensionAssociationId")
            Prelude.<*> (x Data..:? "ExtensionId")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VersionNumber")
      )

instance Prelude.Hashable AppliedExtension where
  hashWithSalt _salt AppliedExtension' {..} =
    _salt `Prelude.hashWithSalt` extensionAssociationId
      `Prelude.hashWithSalt` extensionId
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData AppliedExtension where
  rnf AppliedExtension' {..} =
    Prelude.rnf extensionAssociationId
      `Prelude.seq` Prelude.rnf extensionId
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf versionNumber
