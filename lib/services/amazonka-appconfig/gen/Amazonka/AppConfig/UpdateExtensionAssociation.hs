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
-- Module      : Amazonka.AppConfig.UpdateExtensionAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an association. For more information about extensions and
-- associations, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/working-with-appconfig-extensions.html Working with AppConfig extensions>
-- in the /AppConfig User Guide/.
module Amazonka.AppConfig.UpdateExtensionAssociation
  ( -- * Creating a Request
    UpdateExtensionAssociation (..),
    newUpdateExtensionAssociation,

    -- * Request Lenses
    updateExtensionAssociation_parameters,
    updateExtensionAssociation_extensionAssociationId,

    -- * Destructuring the Response
    ExtensionAssociation (..),
    newExtensionAssociation,

    -- * Response Lenses
    extensionAssociation_arn,
    extensionAssociation_extensionArn,
    extensionAssociation_extensionVersionNumber,
    extensionAssociation_id,
    extensionAssociation_parameters,
    extensionAssociation_resourceArn,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateExtensionAssociation' smart constructor.
data UpdateExtensionAssociation = UpdateExtensionAssociation'
  { -- | The parameter names and values defined in the extension.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The system-generated ID for the association.
    extensionAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExtensionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'updateExtensionAssociation_parameters' - The parameter names and values defined in the extension.
--
-- 'extensionAssociationId', 'updateExtensionAssociation_extensionAssociationId' - The system-generated ID for the association.
newUpdateExtensionAssociation ::
  -- | 'extensionAssociationId'
  Prelude.Text ->
  UpdateExtensionAssociation
newUpdateExtensionAssociation
  pExtensionAssociationId_ =
    UpdateExtensionAssociation'
      { parameters =
          Prelude.Nothing,
        extensionAssociationId =
          pExtensionAssociationId_
      }

-- | The parameter names and values defined in the extension.
updateExtensionAssociation_parameters :: Lens.Lens' UpdateExtensionAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateExtensionAssociation_parameters = Lens.lens (\UpdateExtensionAssociation' {parameters} -> parameters) (\s@UpdateExtensionAssociation' {} a -> s {parameters = a} :: UpdateExtensionAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The system-generated ID for the association.
updateExtensionAssociation_extensionAssociationId :: Lens.Lens' UpdateExtensionAssociation Prelude.Text
updateExtensionAssociation_extensionAssociationId = Lens.lens (\UpdateExtensionAssociation' {extensionAssociationId} -> extensionAssociationId) (\s@UpdateExtensionAssociation' {} a -> s {extensionAssociationId = a} :: UpdateExtensionAssociation)

instance Core.AWSRequest UpdateExtensionAssociation where
  type
    AWSResponse UpdateExtensionAssociation =
      ExtensionAssociation
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateExtensionAssociation where
  hashWithSalt _salt UpdateExtensionAssociation' {..} =
    _salt `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` extensionAssociationId

instance Prelude.NFData UpdateExtensionAssociation where
  rnf UpdateExtensionAssociation' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf extensionAssociationId

instance Data.ToHeaders UpdateExtensionAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateExtensionAssociation where
  toJSON UpdateExtensionAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Parameters" Data..=) Prelude.<$> parameters]
      )

instance Data.ToPath UpdateExtensionAssociation where
  toPath UpdateExtensionAssociation' {..} =
    Prelude.mconcat
      [ "/extensionassociations/",
        Data.toBS extensionAssociationId
      ]

instance Data.ToQuery UpdateExtensionAssociation where
  toQuery = Prelude.const Prelude.mempty
