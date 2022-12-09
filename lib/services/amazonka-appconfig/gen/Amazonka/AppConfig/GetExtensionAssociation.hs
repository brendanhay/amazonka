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
-- Module      : Amazonka.AppConfig.GetExtensionAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an AppConfig extension association. For more
-- information about extensions and associations, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/working-with-appconfig-extensions.html Working with AppConfig extensions>
-- in the /AppConfig User Guide/.
module Amazonka.AppConfig.GetExtensionAssociation
  ( -- * Creating a Request
    GetExtensionAssociation (..),
    newGetExtensionAssociation,

    -- * Request Lenses
    getExtensionAssociation_extensionAssociationId,

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

-- | /See:/ 'newGetExtensionAssociation' smart constructor.
data GetExtensionAssociation = GetExtensionAssociation'
  { -- | The extension association ID to get.
    extensionAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExtensionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extensionAssociationId', 'getExtensionAssociation_extensionAssociationId' - The extension association ID to get.
newGetExtensionAssociation ::
  -- | 'extensionAssociationId'
  Prelude.Text ->
  GetExtensionAssociation
newGetExtensionAssociation pExtensionAssociationId_ =
  GetExtensionAssociation'
    { extensionAssociationId =
        pExtensionAssociationId_
    }

-- | The extension association ID to get.
getExtensionAssociation_extensionAssociationId :: Lens.Lens' GetExtensionAssociation Prelude.Text
getExtensionAssociation_extensionAssociationId = Lens.lens (\GetExtensionAssociation' {extensionAssociationId} -> extensionAssociationId) (\s@GetExtensionAssociation' {} a -> s {extensionAssociationId = a} :: GetExtensionAssociation)

instance Core.AWSRequest GetExtensionAssociation where
  type
    AWSResponse GetExtensionAssociation =
      ExtensionAssociation
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetExtensionAssociation where
  hashWithSalt _salt GetExtensionAssociation' {..} =
    _salt `Prelude.hashWithSalt` extensionAssociationId

instance Prelude.NFData GetExtensionAssociation where
  rnf GetExtensionAssociation' {..} =
    Prelude.rnf extensionAssociationId

instance Data.ToHeaders GetExtensionAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetExtensionAssociation where
  toPath GetExtensionAssociation' {..} =
    Prelude.mconcat
      [ "/extensionassociations/",
        Data.toBS extensionAssociationId
      ]

instance Data.ToQuery GetExtensionAssociation where
  toQuery = Prelude.const Prelude.mempty
