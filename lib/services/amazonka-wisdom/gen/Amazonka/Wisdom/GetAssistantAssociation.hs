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
-- Module      : Amazonka.Wisdom.GetAssistantAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an assistant association.
module Amazonka.Wisdom.GetAssistantAssociation
  ( -- * Creating a Request
    GetAssistantAssociation (..),
    newGetAssistantAssociation,

    -- * Request Lenses
    getAssistantAssociation_assistantAssociationId,
    getAssistantAssociation_assistantId,

    -- * Destructuring the Response
    GetAssistantAssociationResponse (..),
    newGetAssistantAssociationResponse,

    -- * Response Lenses
    getAssistantAssociationResponse_assistantAssociation,
    getAssistantAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newGetAssistantAssociation' smart constructor.
data GetAssistantAssociation = GetAssistantAssociation'
  { -- | The identifier of the assistant association. Can be either the ID or the
    -- ARN. URLs cannot contain the ARN.
    assistantAssociationId :: Prelude.Text,
    -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssistantAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistantAssociationId', 'getAssistantAssociation_assistantAssociationId' - The identifier of the assistant association. Can be either the ID or the
-- ARN. URLs cannot contain the ARN.
--
-- 'assistantId', 'getAssistantAssociation_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newGetAssistantAssociation ::
  -- | 'assistantAssociationId'
  Prelude.Text ->
  -- | 'assistantId'
  Prelude.Text ->
  GetAssistantAssociation
newGetAssistantAssociation
  pAssistantAssociationId_
  pAssistantId_ =
    GetAssistantAssociation'
      { assistantAssociationId =
          pAssistantAssociationId_,
        assistantId = pAssistantId_
      }

-- | The identifier of the assistant association. Can be either the ID or the
-- ARN. URLs cannot contain the ARN.
getAssistantAssociation_assistantAssociationId :: Lens.Lens' GetAssistantAssociation Prelude.Text
getAssistantAssociation_assistantAssociationId = Lens.lens (\GetAssistantAssociation' {assistantAssociationId} -> assistantAssociationId) (\s@GetAssistantAssociation' {} a -> s {assistantAssociationId = a} :: GetAssistantAssociation)

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
getAssistantAssociation_assistantId :: Lens.Lens' GetAssistantAssociation Prelude.Text
getAssistantAssociation_assistantId = Lens.lens (\GetAssistantAssociation' {assistantId} -> assistantId) (\s@GetAssistantAssociation' {} a -> s {assistantId = a} :: GetAssistantAssociation)

instance Core.AWSRequest GetAssistantAssociation where
  type
    AWSResponse GetAssistantAssociation =
      GetAssistantAssociationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssistantAssociationResponse'
            Prelude.<$> (x Core..?> "assistantAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssistantAssociation where
  hashWithSalt _salt GetAssistantAssociation' {..} =
    _salt `Prelude.hashWithSalt` assistantAssociationId
      `Prelude.hashWithSalt` assistantId

instance Prelude.NFData GetAssistantAssociation where
  rnf GetAssistantAssociation' {..} =
    Prelude.rnf assistantAssociationId
      `Prelude.seq` Prelude.rnf assistantId

instance Core.ToHeaders GetAssistantAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetAssistantAssociation where
  toPath GetAssistantAssociation' {..} =
    Prelude.mconcat
      [ "/assistants/",
        Core.toBS assistantId,
        "/associations/",
        Core.toBS assistantAssociationId
      ]

instance Core.ToQuery GetAssistantAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssistantAssociationResponse' smart constructor.
data GetAssistantAssociationResponse = GetAssistantAssociationResponse'
  { -- | The assistant association.
    assistantAssociation :: Prelude.Maybe AssistantAssociationData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssistantAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistantAssociation', 'getAssistantAssociationResponse_assistantAssociation' - The assistant association.
--
-- 'httpStatus', 'getAssistantAssociationResponse_httpStatus' - The response's http status code.
newGetAssistantAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssistantAssociationResponse
newGetAssistantAssociationResponse pHttpStatus_ =
  GetAssistantAssociationResponse'
    { assistantAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The assistant association.
getAssistantAssociationResponse_assistantAssociation :: Lens.Lens' GetAssistantAssociationResponse (Prelude.Maybe AssistantAssociationData)
getAssistantAssociationResponse_assistantAssociation = Lens.lens (\GetAssistantAssociationResponse' {assistantAssociation} -> assistantAssociation) (\s@GetAssistantAssociationResponse' {} a -> s {assistantAssociation = a} :: GetAssistantAssociationResponse)

-- | The response's http status code.
getAssistantAssociationResponse_httpStatus :: Lens.Lens' GetAssistantAssociationResponse Prelude.Int
getAssistantAssociationResponse_httpStatus = Lens.lens (\GetAssistantAssociationResponse' {httpStatus} -> httpStatus) (\s@GetAssistantAssociationResponse' {} a -> s {httpStatus = a} :: GetAssistantAssociationResponse)

instance
  Prelude.NFData
    GetAssistantAssociationResponse
  where
  rnf GetAssistantAssociationResponse' {..} =
    Prelude.rnf assistantAssociation
      `Prelude.seq` Prelude.rnf httpStatus
