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
-- Module      : Amazonka.AppConfig.DeleteExtensionAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an extension association. This action doesn\'t delete extensions
-- defined in the association.
module Amazonka.AppConfig.DeleteExtensionAssociation
  ( -- * Creating a Request
    DeleteExtensionAssociation (..),
    newDeleteExtensionAssociation,

    -- * Request Lenses
    deleteExtensionAssociation_extensionAssociationId,

    -- * Destructuring the Response
    DeleteExtensionAssociationResponse (..),
    newDeleteExtensionAssociationResponse,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteExtensionAssociation' smart constructor.
data DeleteExtensionAssociation = DeleteExtensionAssociation'
  { -- | The ID of the extension association to delete.
    extensionAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExtensionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extensionAssociationId', 'deleteExtensionAssociation_extensionAssociationId' - The ID of the extension association to delete.
newDeleteExtensionAssociation ::
  -- | 'extensionAssociationId'
  Prelude.Text ->
  DeleteExtensionAssociation
newDeleteExtensionAssociation
  pExtensionAssociationId_ =
    DeleteExtensionAssociation'
      { extensionAssociationId =
          pExtensionAssociationId_
      }

-- | The ID of the extension association to delete.
deleteExtensionAssociation_extensionAssociationId :: Lens.Lens' DeleteExtensionAssociation Prelude.Text
deleteExtensionAssociation_extensionAssociationId = Lens.lens (\DeleteExtensionAssociation' {extensionAssociationId} -> extensionAssociationId) (\s@DeleteExtensionAssociation' {} a -> s {extensionAssociationId = a} :: DeleteExtensionAssociation)

instance Core.AWSRequest DeleteExtensionAssociation where
  type
    AWSResponse DeleteExtensionAssociation =
      DeleteExtensionAssociationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteExtensionAssociationResponse'

instance Prelude.Hashable DeleteExtensionAssociation where
  hashWithSalt _salt DeleteExtensionAssociation' {..} =
    _salt `Prelude.hashWithSalt` extensionAssociationId

instance Prelude.NFData DeleteExtensionAssociation where
  rnf DeleteExtensionAssociation' {..} =
    Prelude.rnf extensionAssociationId

instance Core.ToHeaders DeleteExtensionAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteExtensionAssociation where
  toPath DeleteExtensionAssociation' {..} =
    Prelude.mconcat
      [ "/extensionassociations/",
        Core.toBS extensionAssociationId
      ]

instance Core.ToQuery DeleteExtensionAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteExtensionAssociationResponse' smart constructor.
data DeleteExtensionAssociationResponse = DeleteExtensionAssociationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExtensionAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteExtensionAssociationResponse ::
  DeleteExtensionAssociationResponse
newDeleteExtensionAssociationResponse =
  DeleteExtensionAssociationResponse'

instance
  Prelude.NFData
    DeleteExtensionAssociationResponse
  where
  rnf _ = ()
