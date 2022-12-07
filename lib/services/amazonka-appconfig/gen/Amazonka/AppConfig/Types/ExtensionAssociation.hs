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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.ExtensionAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newExtensionAssociation' smart constructor.
data ExtensionAssociation = ExtensionAssociation'
  { -- | The version number for the extension defined in the association.
    extensionVersionNumber :: Prelude.Maybe Prelude.Int,
    -- | The system-generated Amazon Resource Name (ARN) for the extension.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID for the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of applications, configuration profiles, or environments
    -- defined in the association.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the extension defined in the association.
    extensionArn :: Prelude.Maybe Prelude.Text,
    -- | The parameter names and values defined in the association.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'extensionVersionNumber', 'extensionAssociation_extensionVersionNumber' - The version number for the extension defined in the association.
--
-- 'arn', 'extensionAssociation_arn' - The system-generated Amazon Resource Name (ARN) for the extension.
--
-- 'id', 'extensionAssociation_id' - The system-generated ID for the association.
--
-- 'resourceArn', 'extensionAssociation_resourceArn' - The ARNs of applications, configuration profiles, or environments
-- defined in the association.
--
-- 'extensionArn', 'extensionAssociation_extensionArn' - The ARN of the extension defined in the association.
--
-- 'parameters', 'extensionAssociation_parameters' - The parameter names and values defined in the association.
newExtensionAssociation ::
  ExtensionAssociation
newExtensionAssociation =
  ExtensionAssociation'
    { extensionVersionNumber =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      extensionArn = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The version number for the extension defined in the association.
extensionAssociation_extensionVersionNumber :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Int)
extensionAssociation_extensionVersionNumber = Lens.lens (\ExtensionAssociation' {extensionVersionNumber} -> extensionVersionNumber) (\s@ExtensionAssociation' {} a -> s {extensionVersionNumber = a} :: ExtensionAssociation)

-- | The system-generated Amazon Resource Name (ARN) for the extension.
extensionAssociation_arn :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Text)
extensionAssociation_arn = Lens.lens (\ExtensionAssociation' {arn} -> arn) (\s@ExtensionAssociation' {} a -> s {arn = a} :: ExtensionAssociation)

-- | The system-generated ID for the association.
extensionAssociation_id :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Text)
extensionAssociation_id = Lens.lens (\ExtensionAssociation' {id} -> id) (\s@ExtensionAssociation' {} a -> s {id = a} :: ExtensionAssociation)

-- | The ARNs of applications, configuration profiles, or environments
-- defined in the association.
extensionAssociation_resourceArn :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Text)
extensionAssociation_resourceArn = Lens.lens (\ExtensionAssociation' {resourceArn} -> resourceArn) (\s@ExtensionAssociation' {} a -> s {resourceArn = a} :: ExtensionAssociation)

-- | The ARN of the extension defined in the association.
extensionAssociation_extensionArn :: Lens.Lens' ExtensionAssociation (Prelude.Maybe Prelude.Text)
extensionAssociation_extensionArn = Lens.lens (\ExtensionAssociation' {extensionArn} -> extensionArn) (\s@ExtensionAssociation' {} a -> s {extensionArn = a} :: ExtensionAssociation)

-- | The parameter names and values defined in the association.
extensionAssociation_parameters :: Lens.Lens' ExtensionAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
extensionAssociation_parameters = Lens.lens (\ExtensionAssociation' {parameters} -> parameters) (\s@ExtensionAssociation' {} a -> s {parameters = a} :: ExtensionAssociation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExtensionAssociation where
  parseJSON =
    Data.withObject
      "ExtensionAssociation"
      ( \x ->
          ExtensionAssociation'
            Prelude.<$> (x Data..:? "ExtensionVersionNumber")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ExtensionArn")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExtensionAssociation where
  hashWithSalt _salt ExtensionAssociation' {..} =
    _salt `Prelude.hashWithSalt` extensionVersionNumber
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` extensionArn
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData ExtensionAssociation where
  rnf ExtensionAssociation' {..} =
    Prelude.rnf extensionVersionNumber
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf extensionArn
      `Prelude.seq` Prelude.rnf parameters
