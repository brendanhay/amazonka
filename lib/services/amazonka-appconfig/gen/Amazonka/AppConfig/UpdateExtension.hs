{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppConfig.UpdateExtension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an AppConfig extension. For more information about extensions,
-- see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/working-with-appconfig-extensions.html Working with AppConfig extensions>
-- in the /AppConfig User Guide/.
module Amazonka.AppConfig.UpdateExtension
  ( -- * Creating a Request
    UpdateExtension (..),
    newUpdateExtension,

    -- * Request Lenses
    updateExtension_actions,
    updateExtension_description,
    updateExtension_parameters,
    updateExtension_versionNumber,
    updateExtension_extensionIdentifier,

    -- * Destructuring the Response
    Extension (..),
    newExtension,

    -- * Response Lenses
    extension_actions,
    extension_arn,
    extension_description,
    extension_id,
    extension_name,
    extension_parameters,
    extension_versionNumber,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateExtension' smart constructor.
data UpdateExtension = UpdateExtension'
  { -- | The actions defined in the extension.
    actions :: Prelude.Maybe (Prelude.HashMap ActionPoint (Prelude.NonEmpty Action)),
    -- | Information about the extension.
    description :: Prelude.Maybe Prelude.Text,
    -- | One or more parameters for the actions called by the extension.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Parameter),
    -- | The extension version number.
    versionNumber :: Prelude.Maybe Prelude.Int,
    -- | The name, the ID, or the Amazon Resource Name (ARN) of the extension.
    extensionIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExtension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'updateExtension_actions' - The actions defined in the extension.
--
-- 'description', 'updateExtension_description' - Information about the extension.
--
-- 'parameters', 'updateExtension_parameters' - One or more parameters for the actions called by the extension.
--
-- 'versionNumber', 'updateExtension_versionNumber' - The extension version number.
--
-- 'extensionIdentifier', 'updateExtension_extensionIdentifier' - The name, the ID, or the Amazon Resource Name (ARN) of the extension.
newUpdateExtension ::
  -- | 'extensionIdentifier'
  Prelude.Text ->
  UpdateExtension
newUpdateExtension pExtensionIdentifier_ =
  UpdateExtension'
    { actions = Prelude.Nothing,
      description = Prelude.Nothing,
      parameters = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      extensionIdentifier = pExtensionIdentifier_
    }

-- | The actions defined in the extension.
updateExtension_actions :: Lens.Lens' UpdateExtension (Prelude.Maybe (Prelude.HashMap ActionPoint (Prelude.NonEmpty Action)))
updateExtension_actions = Lens.lens (\UpdateExtension' {actions} -> actions) (\s@UpdateExtension' {} a -> s {actions = a} :: UpdateExtension) Prelude.. Lens.mapping Lens.coerced

-- | Information about the extension.
updateExtension_description :: Lens.Lens' UpdateExtension (Prelude.Maybe Prelude.Text)
updateExtension_description = Lens.lens (\UpdateExtension' {description} -> description) (\s@UpdateExtension' {} a -> s {description = a} :: UpdateExtension)

-- | One or more parameters for the actions called by the extension.
updateExtension_parameters :: Lens.Lens' UpdateExtension (Prelude.Maybe (Prelude.HashMap Prelude.Text Parameter))
updateExtension_parameters = Lens.lens (\UpdateExtension' {parameters} -> parameters) (\s@UpdateExtension' {} a -> s {parameters = a} :: UpdateExtension) Prelude.. Lens.mapping Lens.coerced

-- | The extension version number.
updateExtension_versionNumber :: Lens.Lens' UpdateExtension (Prelude.Maybe Prelude.Int)
updateExtension_versionNumber = Lens.lens (\UpdateExtension' {versionNumber} -> versionNumber) (\s@UpdateExtension' {} a -> s {versionNumber = a} :: UpdateExtension)

-- | The name, the ID, or the Amazon Resource Name (ARN) of the extension.
updateExtension_extensionIdentifier :: Lens.Lens' UpdateExtension Prelude.Text
updateExtension_extensionIdentifier = Lens.lens (\UpdateExtension' {extensionIdentifier} -> extensionIdentifier) (\s@UpdateExtension' {} a -> s {extensionIdentifier = a} :: UpdateExtension)

instance Core.AWSRequest UpdateExtension where
  type AWSResponse UpdateExtension = Extension
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateExtension where
  hashWithSalt _salt UpdateExtension' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` extensionIdentifier

instance Prelude.NFData UpdateExtension where
  rnf UpdateExtension' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf extensionIdentifier

instance Data.ToHeaders UpdateExtension where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateExtension where
  toJSON UpdateExtension' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("Description" Data..=) Prelude.<$> description,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("VersionNumber" Data..=) Prelude.<$> versionNumber
          ]
      )

instance Data.ToPath UpdateExtension where
  toPath UpdateExtension' {..} =
    Prelude.mconcat
      ["/extensions/", Data.toBS extensionIdentifier]

instance Data.ToQuery UpdateExtension where
  toQuery = Prelude.const Prelude.mempty
