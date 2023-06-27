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
-- Module      : Amazonka.CleanRooms.UpdateCollaboration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates collaboration metadata and can only be called by the
-- collaboration owner.
module Amazonka.CleanRooms.UpdateCollaboration
  ( -- * Creating a Request
    UpdateCollaboration (..),
    newUpdateCollaboration,

    -- * Request Lenses
    updateCollaboration_description,
    updateCollaboration_name,
    updateCollaboration_collaborationIdentifier,

    -- * Destructuring the Response
    UpdateCollaborationResponse (..),
    newUpdateCollaborationResponse,

    -- * Response Lenses
    updateCollaborationResponse_httpStatus,
    updateCollaborationResponse_collaboration,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCollaboration' smart constructor.
data UpdateCollaboration = UpdateCollaboration'
  { -- | A description of the collaboration.
    description :: Prelude.Maybe Prelude.Text,
    -- | A human-readable identifier provided by the collaboration owner. Display
    -- names are not unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the collaboration.
    collaborationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCollaboration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateCollaboration_description' - A description of the collaboration.
--
-- 'name', 'updateCollaboration_name' - A human-readable identifier provided by the collaboration owner. Display
-- names are not unique.
--
-- 'collaborationIdentifier', 'updateCollaboration_collaborationIdentifier' - The identifier for the collaboration.
newUpdateCollaboration ::
  -- | 'collaborationIdentifier'
  Prelude.Text ->
  UpdateCollaboration
newUpdateCollaboration pCollaborationIdentifier_ =
  UpdateCollaboration'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      collaborationIdentifier = pCollaborationIdentifier_
    }

-- | A description of the collaboration.
updateCollaboration_description :: Lens.Lens' UpdateCollaboration (Prelude.Maybe Prelude.Text)
updateCollaboration_description = Lens.lens (\UpdateCollaboration' {description} -> description) (\s@UpdateCollaboration' {} a -> s {description = a} :: UpdateCollaboration)

-- | A human-readable identifier provided by the collaboration owner. Display
-- names are not unique.
updateCollaboration_name :: Lens.Lens' UpdateCollaboration (Prelude.Maybe Prelude.Text)
updateCollaboration_name = Lens.lens (\UpdateCollaboration' {name} -> name) (\s@UpdateCollaboration' {} a -> s {name = a} :: UpdateCollaboration)

-- | The identifier for the collaboration.
updateCollaboration_collaborationIdentifier :: Lens.Lens' UpdateCollaboration Prelude.Text
updateCollaboration_collaborationIdentifier = Lens.lens (\UpdateCollaboration' {collaborationIdentifier} -> collaborationIdentifier) (\s@UpdateCollaboration' {} a -> s {collaborationIdentifier = a} :: UpdateCollaboration)

instance Core.AWSRequest UpdateCollaboration where
  type
    AWSResponse UpdateCollaboration =
      UpdateCollaborationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCollaborationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "collaboration")
      )

instance Prelude.Hashable UpdateCollaboration where
  hashWithSalt _salt UpdateCollaboration' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` collaborationIdentifier

instance Prelude.NFData UpdateCollaboration where
  rnf UpdateCollaboration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf collaborationIdentifier

instance Data.ToHeaders UpdateCollaboration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCollaboration where
  toJSON UpdateCollaboration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateCollaboration where
  toPath UpdateCollaboration' {..} =
    Prelude.mconcat
      [ "/collaborations/",
        Data.toBS collaborationIdentifier
      ]

instance Data.ToQuery UpdateCollaboration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCollaborationResponse' smart constructor.
data UpdateCollaborationResponse = UpdateCollaborationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire collaboration that has been updated.
    collaboration :: Collaboration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCollaborationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCollaborationResponse_httpStatus' - The response's http status code.
--
-- 'collaboration', 'updateCollaborationResponse_collaboration' - The entire collaboration that has been updated.
newUpdateCollaborationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'collaboration'
  Collaboration ->
  UpdateCollaborationResponse
newUpdateCollaborationResponse
  pHttpStatus_
  pCollaboration_ =
    UpdateCollaborationResponse'
      { httpStatus =
          pHttpStatus_,
        collaboration = pCollaboration_
      }

-- | The response's http status code.
updateCollaborationResponse_httpStatus :: Lens.Lens' UpdateCollaborationResponse Prelude.Int
updateCollaborationResponse_httpStatus = Lens.lens (\UpdateCollaborationResponse' {httpStatus} -> httpStatus) (\s@UpdateCollaborationResponse' {} a -> s {httpStatus = a} :: UpdateCollaborationResponse)

-- | The entire collaboration that has been updated.
updateCollaborationResponse_collaboration :: Lens.Lens' UpdateCollaborationResponse Collaboration
updateCollaborationResponse_collaboration = Lens.lens (\UpdateCollaborationResponse' {collaboration} -> collaboration) (\s@UpdateCollaborationResponse' {} a -> s {collaboration = a} :: UpdateCollaborationResponse)

instance Prelude.NFData UpdateCollaborationResponse where
  rnf UpdateCollaborationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf collaboration
