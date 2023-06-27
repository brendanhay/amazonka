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
-- Module      : Amazonka.CleanRooms.DeleteCollaboration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a collaboration. It can only be called by the collaboration
-- owner.
module Amazonka.CleanRooms.DeleteCollaboration
  ( -- * Creating a Request
    DeleteCollaboration (..),
    newDeleteCollaboration,

    -- * Request Lenses
    deleteCollaboration_collaborationIdentifier,

    -- * Destructuring the Response
    DeleteCollaborationResponse (..),
    newDeleteCollaborationResponse,

    -- * Response Lenses
    deleteCollaborationResponse_httpStatus,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCollaboration' smart constructor.
data DeleteCollaboration = DeleteCollaboration'
  { -- | The identifier for the collaboration.
    collaborationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCollaboration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collaborationIdentifier', 'deleteCollaboration_collaborationIdentifier' - The identifier for the collaboration.
newDeleteCollaboration ::
  -- | 'collaborationIdentifier'
  Prelude.Text ->
  DeleteCollaboration
newDeleteCollaboration pCollaborationIdentifier_ =
  DeleteCollaboration'
    { collaborationIdentifier =
        pCollaborationIdentifier_
    }

-- | The identifier for the collaboration.
deleteCollaboration_collaborationIdentifier :: Lens.Lens' DeleteCollaboration Prelude.Text
deleteCollaboration_collaborationIdentifier = Lens.lens (\DeleteCollaboration' {collaborationIdentifier} -> collaborationIdentifier) (\s@DeleteCollaboration' {} a -> s {collaborationIdentifier = a} :: DeleteCollaboration)

instance Core.AWSRequest DeleteCollaboration where
  type
    AWSResponse DeleteCollaboration =
      DeleteCollaborationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCollaborationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCollaboration where
  hashWithSalt _salt DeleteCollaboration' {..} =
    _salt
      `Prelude.hashWithSalt` collaborationIdentifier

instance Prelude.NFData DeleteCollaboration where
  rnf DeleteCollaboration' {..} =
    Prelude.rnf collaborationIdentifier

instance Data.ToHeaders DeleteCollaboration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCollaboration where
  toPath DeleteCollaboration' {..} =
    Prelude.mconcat
      [ "/collaborations/",
        Data.toBS collaborationIdentifier
      ]

instance Data.ToQuery DeleteCollaboration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCollaborationResponse' smart constructor.
data DeleteCollaborationResponse = DeleteCollaborationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCollaborationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCollaborationResponse_httpStatus' - The response's http status code.
newDeleteCollaborationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCollaborationResponse
newDeleteCollaborationResponse pHttpStatus_ =
  DeleteCollaborationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCollaborationResponse_httpStatus :: Lens.Lens' DeleteCollaborationResponse Prelude.Int
deleteCollaborationResponse_httpStatus = Lens.lens (\DeleteCollaborationResponse' {httpStatus} -> httpStatus) (\s@DeleteCollaborationResponse' {} a -> s {httpStatus = a} :: DeleteCollaborationResponse)

instance Prelude.NFData DeleteCollaborationResponse where
  rnf DeleteCollaborationResponse' {..} =
    Prelude.rnf httpStatus
