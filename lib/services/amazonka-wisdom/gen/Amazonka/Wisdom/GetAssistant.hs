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
-- Module      : Amazonka.Wisdom.GetAssistant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an assistant.
module Amazonka.Wisdom.GetAssistant
  ( -- * Creating a Request
    GetAssistant (..),
    newGetAssistant,

    -- * Request Lenses
    getAssistant_assistantId,

    -- * Destructuring the Response
    GetAssistantResponse (..),
    newGetAssistantResponse,

    -- * Response Lenses
    getAssistantResponse_assistant,
    getAssistantResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newGetAssistant' smart constructor.
data GetAssistant = GetAssistant'
  { -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssistant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistantId', 'getAssistant_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newGetAssistant ::
  -- | 'assistantId'
  Prelude.Text ->
  GetAssistant
newGetAssistant pAssistantId_ =
  GetAssistant' {assistantId = pAssistantId_}

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
getAssistant_assistantId :: Lens.Lens' GetAssistant Prelude.Text
getAssistant_assistantId = Lens.lens (\GetAssistant' {assistantId} -> assistantId) (\s@GetAssistant' {} a -> s {assistantId = a} :: GetAssistant)

instance Core.AWSRequest GetAssistant where
  type AWSResponse GetAssistant = GetAssistantResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssistantResponse'
            Prelude.<$> (x Core..?> "assistant")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssistant where
  hashWithSalt _salt GetAssistant' {..} =
    _salt `Prelude.hashWithSalt` assistantId

instance Prelude.NFData GetAssistant where
  rnf GetAssistant' {..} = Prelude.rnf assistantId

instance Core.ToHeaders GetAssistant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetAssistant where
  toPath GetAssistant' {..} =
    Prelude.mconcat
      ["/assistants/", Core.toBS assistantId]

instance Core.ToQuery GetAssistant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssistantResponse' smart constructor.
data GetAssistantResponse = GetAssistantResponse'
  { -- | Information about the assistant.
    assistant :: Prelude.Maybe AssistantData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssistantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistant', 'getAssistantResponse_assistant' - Information about the assistant.
--
-- 'httpStatus', 'getAssistantResponse_httpStatus' - The response's http status code.
newGetAssistantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssistantResponse
newGetAssistantResponse pHttpStatus_ =
  GetAssistantResponse'
    { assistant = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the assistant.
getAssistantResponse_assistant :: Lens.Lens' GetAssistantResponse (Prelude.Maybe AssistantData)
getAssistantResponse_assistant = Lens.lens (\GetAssistantResponse' {assistant} -> assistant) (\s@GetAssistantResponse' {} a -> s {assistant = a} :: GetAssistantResponse)

-- | The response's http status code.
getAssistantResponse_httpStatus :: Lens.Lens' GetAssistantResponse Prelude.Int
getAssistantResponse_httpStatus = Lens.lens (\GetAssistantResponse' {httpStatus} -> httpStatus) (\s@GetAssistantResponse' {} a -> s {httpStatus = a} :: GetAssistantResponse)

instance Prelude.NFData GetAssistantResponse where
  rnf GetAssistantResponse' {..} =
    Prelude.rnf assistant
      `Prelude.seq` Prelude.rnf httpStatus
