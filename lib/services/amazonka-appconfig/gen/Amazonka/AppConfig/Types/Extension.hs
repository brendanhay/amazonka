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
-- Module      : Amazonka.AppConfig.Types.Extension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Extension where

import Amazonka.AppConfig.Types.Action
import Amazonka.AppConfig.Types.ActionPoint
import Amazonka.AppConfig.Types.Parameter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newExtension' smart constructor.
data Extension = Extension'
  { -- | The extension name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The system-generated Amazon Resource Name (ARN) for the extension.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the extension.
    id :: Prelude.Maybe Prelude.Text,
    -- | Information about the extension.
    description :: Prelude.Maybe Prelude.Text,
    -- | The extension version number.
    versionNumber :: Prelude.Maybe Prelude.Int,
    -- | The actions defined in the extension.
    actions :: Prelude.Maybe (Prelude.HashMap ActionPoint (Prelude.NonEmpty Action)),
    -- | The parameters accepted by the extension. You specify parameter values
    -- when you associate the extension to an AppConfig resource by using the
    -- @CreateExtensionAssociation@ API action. For Lambda extension actions,
    -- these parameters are included in the Lambda request object.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Parameter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Extension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'extension_name' - The extension name.
--
-- 'arn', 'extension_arn' - The system-generated Amazon Resource Name (ARN) for the extension.
--
-- 'id', 'extension_id' - The system-generated ID of the extension.
--
-- 'description', 'extension_description' - Information about the extension.
--
-- 'versionNumber', 'extension_versionNumber' - The extension version number.
--
-- 'actions', 'extension_actions' - The actions defined in the extension.
--
-- 'parameters', 'extension_parameters' - The parameters accepted by the extension. You specify parameter values
-- when you associate the extension to an AppConfig resource by using the
-- @CreateExtensionAssociation@ API action. For Lambda extension actions,
-- these parameters are included in the Lambda request object.
newExtension ::
  Extension
newExtension =
  Extension'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      actions = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The extension name.
extension_name :: Lens.Lens' Extension (Prelude.Maybe Prelude.Text)
extension_name = Lens.lens (\Extension' {name} -> name) (\s@Extension' {} a -> s {name = a} :: Extension)

-- | The system-generated Amazon Resource Name (ARN) for the extension.
extension_arn :: Lens.Lens' Extension (Prelude.Maybe Prelude.Text)
extension_arn = Lens.lens (\Extension' {arn} -> arn) (\s@Extension' {} a -> s {arn = a} :: Extension)

-- | The system-generated ID of the extension.
extension_id :: Lens.Lens' Extension (Prelude.Maybe Prelude.Text)
extension_id = Lens.lens (\Extension' {id} -> id) (\s@Extension' {} a -> s {id = a} :: Extension)

-- | Information about the extension.
extension_description :: Lens.Lens' Extension (Prelude.Maybe Prelude.Text)
extension_description = Lens.lens (\Extension' {description} -> description) (\s@Extension' {} a -> s {description = a} :: Extension)

-- | The extension version number.
extension_versionNumber :: Lens.Lens' Extension (Prelude.Maybe Prelude.Int)
extension_versionNumber = Lens.lens (\Extension' {versionNumber} -> versionNumber) (\s@Extension' {} a -> s {versionNumber = a} :: Extension)

-- | The actions defined in the extension.
extension_actions :: Lens.Lens' Extension (Prelude.Maybe (Prelude.HashMap ActionPoint (Prelude.NonEmpty Action)))
extension_actions = Lens.lens (\Extension' {actions} -> actions) (\s@Extension' {} a -> s {actions = a} :: Extension) Prelude.. Lens.mapping Lens.coerced

-- | The parameters accepted by the extension. You specify parameter values
-- when you associate the extension to an AppConfig resource by using the
-- @CreateExtensionAssociation@ API action. For Lambda extension actions,
-- these parameters are included in the Lambda request object.
extension_parameters :: Lens.Lens' Extension (Prelude.Maybe (Prelude.HashMap Prelude.Text Parameter))
extension_parameters = Lens.lens (\Extension' {parameters} -> parameters) (\s@Extension' {} a -> s {parameters = a} :: Extension) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Extension where
  parseJSON =
    Core.withObject
      "Extension"
      ( \x ->
          Extension'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "VersionNumber")
            Prelude.<*> (x Core..:? "Actions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Extension where
  hashWithSalt _salt Extension' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData Extension where
  rnf Extension' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf parameters
