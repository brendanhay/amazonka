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
-- Module      : Amazonka.AppConfig.Types.ExtensionAssociationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.ExtensionAssociationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an association between an extension and an AppConfig
-- resource such as an application, environment, or configuration profile.
-- Call @GetExtensionAssociation@ to get more information about an
-- association.
--
-- /See:/ 'newExtensionAssociationSummary' smart constructor.
data ExtensionAssociationSummary = ExtensionAssociationSummary'
  { -- | The extension association ID. This ID is used to call other
    -- @ExtensionAssociation@ API actions such as @GetExtensionAssociation@ or
    -- @DeleteExtensionAssociation@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of applications, configuration profiles, or environments
    -- defined in the association.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The system-generated Amazon Resource Name (ARN) for the extension.
    extensionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtensionAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'extensionAssociationSummary_id' - The extension association ID. This ID is used to call other
-- @ExtensionAssociation@ API actions such as @GetExtensionAssociation@ or
-- @DeleteExtensionAssociation@.
--
-- 'resourceArn', 'extensionAssociationSummary_resourceArn' - The ARNs of applications, configuration profiles, or environments
-- defined in the association.
--
-- 'extensionArn', 'extensionAssociationSummary_extensionArn' - The system-generated Amazon Resource Name (ARN) for the extension.
newExtensionAssociationSummary ::
  ExtensionAssociationSummary
newExtensionAssociationSummary =
  ExtensionAssociationSummary'
    { id = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      extensionArn = Prelude.Nothing
    }

-- | The extension association ID. This ID is used to call other
-- @ExtensionAssociation@ API actions such as @GetExtensionAssociation@ or
-- @DeleteExtensionAssociation@.
extensionAssociationSummary_id :: Lens.Lens' ExtensionAssociationSummary (Prelude.Maybe Prelude.Text)
extensionAssociationSummary_id = Lens.lens (\ExtensionAssociationSummary' {id} -> id) (\s@ExtensionAssociationSummary' {} a -> s {id = a} :: ExtensionAssociationSummary)

-- | The ARNs of applications, configuration profiles, or environments
-- defined in the association.
extensionAssociationSummary_resourceArn :: Lens.Lens' ExtensionAssociationSummary (Prelude.Maybe Prelude.Text)
extensionAssociationSummary_resourceArn = Lens.lens (\ExtensionAssociationSummary' {resourceArn} -> resourceArn) (\s@ExtensionAssociationSummary' {} a -> s {resourceArn = a} :: ExtensionAssociationSummary)

-- | The system-generated Amazon Resource Name (ARN) for the extension.
extensionAssociationSummary_extensionArn :: Lens.Lens' ExtensionAssociationSummary (Prelude.Maybe Prelude.Text)
extensionAssociationSummary_extensionArn = Lens.lens (\ExtensionAssociationSummary' {extensionArn} -> extensionArn) (\s@ExtensionAssociationSummary' {} a -> s {extensionArn = a} :: ExtensionAssociationSummary)

instance Core.FromJSON ExtensionAssociationSummary where
  parseJSON =
    Core.withObject
      "ExtensionAssociationSummary"
      ( \x ->
          ExtensionAssociationSummary'
            Prelude.<$> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "ExtensionArn")
      )

instance Prelude.Hashable ExtensionAssociationSummary where
  hashWithSalt _salt ExtensionAssociationSummary' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` extensionArn

instance Prelude.NFData ExtensionAssociationSummary where
  rnf ExtensionAssociationSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf extensionArn
