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
-- Module      : Amazonka.CleanRooms.GetCollaboration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a collaboration.
module Amazonka.CleanRooms.GetCollaboration
  ( -- * Creating a Request
    GetCollaboration (..),
    newGetCollaboration,

    -- * Request Lenses
    getCollaboration_collaborationIdentifier,

    -- * Destructuring the Response
    GetCollaborationResponse (..),
    newGetCollaborationResponse,

    -- * Response Lenses
    getCollaborationResponse_httpStatus,
    getCollaborationResponse_collaboration,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCollaboration' smart constructor.
data GetCollaboration = GetCollaboration'
  { -- | The identifier for the collaboration.
    collaborationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCollaboration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collaborationIdentifier', 'getCollaboration_collaborationIdentifier' - The identifier for the collaboration.
newGetCollaboration ::
  -- | 'collaborationIdentifier'
  Prelude.Text ->
  GetCollaboration
newGetCollaboration pCollaborationIdentifier_ =
  GetCollaboration'
    { collaborationIdentifier =
        pCollaborationIdentifier_
    }

-- | The identifier for the collaboration.
getCollaboration_collaborationIdentifier :: Lens.Lens' GetCollaboration Prelude.Text
getCollaboration_collaborationIdentifier = Lens.lens (\GetCollaboration' {collaborationIdentifier} -> collaborationIdentifier) (\s@GetCollaboration' {} a -> s {collaborationIdentifier = a} :: GetCollaboration)

instance Core.AWSRequest GetCollaboration where
  type
    AWSResponse GetCollaboration =
      GetCollaborationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCollaborationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "collaboration")
      )

instance Prelude.Hashable GetCollaboration where
  hashWithSalt _salt GetCollaboration' {..} =
    _salt
      `Prelude.hashWithSalt` collaborationIdentifier

instance Prelude.NFData GetCollaboration where
  rnf GetCollaboration' {..} =
    Prelude.rnf collaborationIdentifier

instance Data.ToHeaders GetCollaboration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCollaboration where
  toPath GetCollaboration' {..} =
    Prelude.mconcat
      [ "/collaborations/",
        Data.toBS collaborationIdentifier
      ]

instance Data.ToQuery GetCollaboration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCollaborationResponse' smart constructor.
data GetCollaborationResponse = GetCollaborationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire collaboration for this identifier.
    collaboration :: Collaboration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCollaborationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCollaborationResponse_httpStatus' - The response's http status code.
--
-- 'collaboration', 'getCollaborationResponse_collaboration' - The entire collaboration for this identifier.
newGetCollaborationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'collaboration'
  Collaboration ->
  GetCollaborationResponse
newGetCollaborationResponse
  pHttpStatus_
  pCollaboration_ =
    GetCollaborationResponse'
      { httpStatus =
          pHttpStatus_,
        collaboration = pCollaboration_
      }

-- | The response's http status code.
getCollaborationResponse_httpStatus :: Lens.Lens' GetCollaborationResponse Prelude.Int
getCollaborationResponse_httpStatus = Lens.lens (\GetCollaborationResponse' {httpStatus} -> httpStatus) (\s@GetCollaborationResponse' {} a -> s {httpStatus = a} :: GetCollaborationResponse)

-- | The entire collaboration for this identifier.
getCollaborationResponse_collaboration :: Lens.Lens' GetCollaborationResponse Collaboration
getCollaborationResponse_collaboration = Lens.lens (\GetCollaborationResponse' {collaboration} -> collaboration) (\s@GetCollaborationResponse' {} a -> s {collaboration = a} :: GetCollaborationResponse)

instance Prelude.NFData GetCollaborationResponse where
  rnf GetCollaborationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf collaboration
