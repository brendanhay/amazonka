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
-- Module      : Amazonka.AppConfig.Types.ExtensionAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.ExtensionAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newExtensionAssociation' smart constructor.
data ExtensionAssociation = ExtensionAssociation'
  { -- | The system-generated Amazon Resource Name (ARN) for the extension.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the extension defined in the association.
    extensionArn :: Prelude.Maybe Prelude.Text,
    -- | The version number for the extension defined in the association.
    extensionVersionNumber :: Prelude.Maybe Prelude.Int,
    -- | The system-generated ID for the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The parameter names and values defined in the association.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARNs of applications, configuration profiles, or environments
    -- defined in the association.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtensionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'extensionAssociation_arn' - The system-generated Amazon Resource Name (ARN) for the extension.
--
-- 'extensionArn', 'extensionAssociation_extensionArn' - The ARN of the extension defined in the association.
--
-- 'extensionVersionNumber', 'extensionAssociation_extensionVersionNumber' - The version number for the extension defined in the association.
--
-- 'id', 'extensionAssociation_id' - The system-generated ID for the association.
--
-- 'parameters', 'extensionAssociation_parameters' - The parameter names and values defined in the association.
--
-- 'resourceArn', 'extensionAssociation_resourceArn' - The ARNs of applications, configuration profiles, or environments
-- defined in the association.
newExtensionAssociation ::
  ExtensionAssociation
newExtensionAssociation =
  ExtensionAssociation'
    { arn = Prelude.Nothing,
      extensionArn = Prelude.Nothing,
      extensionVersionNumber = Prelude.Nothing,
      id = Prelude.Nothing,
      parameters = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The system-generated Amazon Resource Name (ARN) for the extension.
extensionAssociation_arn :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Text)
extensionAssociation_arn = Lens.lens (\ExtensionAssociation' {arn} -> arn) (\s@ExtensionAssociation' {} a -> s {arn = a} :: ExtensionAssociation)

-- | The ARN of the extension defined in the association.
extensionAssociation_extensionArn :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Text)
extensionAssociation_extensionArn = Lens.lens (\ExtensionAssociation' {extensionArn} -> extensionArn) (\s@ExtensionAssociation' {} a -> s {extensionArn = a} :: ExtensionAssociation)

-- | The version number for the extension defined in the association.
extensionAssociation_extensionVersionNumber :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Int)
extensionAssociation_extensionVersionNumber = Lens.lens (\ExtensionAssociation' {extensionVersionNumber} -> extensionVersionNumber) (\s@ExtensionAssociation' {} a -> s {extensionVersionNumber = a} :: ExtensionAssociation)

-- | The system-generated ID for the association.
extensionAssociation_id :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Text)
extensionAssociation_id = Lens.lens (\ExtensionAssociation' {id} -> id) (\s@ExtensionAssociation' {} a -> s {id = a} :: ExtensionAssociation)

-- | The parameter names and values defined in the association.
extensionAssociation_parameters :: Lens.Lens' ExtensionAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
extensionAssociation_parameters = Lens.lens (\ExtensionAssociation' {parameters} -> parameters) (\s@ExtensionAssociation' {} a -> s {parameters = a} :: ExtensionAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ARNs of applications, configuration profiles, or environments
-- defined in the association.
extensionAssociation_resourceArn :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Text)
extensionAssociation_resourceArn = Lens.lens (\ExtensionAssociation' {resourceArn} -> resourceArn) (\s@ExtensionAssociation' {} a -> s {resourceArn = a} :: ExtensionAssociation)

instance Data.FromJSON ExtensionAssociation where
  parseJSON =
    Data.withObject
      "ExtensionAssociation"
      ( \x ->
          ExtensionAssociation'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "ExtensionArn")
            Prelude.<*> (x Data..:? "ExtensionVersionNumber")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ResourceArn")
      )

instance Prelude.Hashable ExtensionAssociation where
  hashWithSalt _salt ExtensionAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` extensionArn
      `Prelude.hashWithSalt` extensionVersionNumber
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ExtensionAssociation where
  rnf ExtensionAssociation' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf extensionArn
      `Prelude.seq` Prelude.rnf extensionVersionNumber
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf resourceArn
