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
-- Module      : Amazonka.Connect.GetPromptFile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the prompt file.
module Amazonka.Connect.GetPromptFile
  ( -- * Creating a Request
    GetPromptFile (..),
    newGetPromptFile,

    -- * Request Lenses
    getPromptFile_instanceId,
    getPromptFile_promptId,

    -- * Destructuring the Response
    GetPromptFileResponse (..),
    newGetPromptFileResponse,

    -- * Response Lenses
    getPromptFileResponse_promptPresignedUrl,
    getPromptFileResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPromptFile' smart constructor.
data GetPromptFile = GetPromptFile'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the prompt.
    promptId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPromptFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'getPromptFile_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'promptId', 'getPromptFile_promptId' - A unique identifier for the prompt.
newGetPromptFile ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'promptId'
  Prelude.Text ->
  GetPromptFile
newGetPromptFile pInstanceId_ pPromptId_ =
  GetPromptFile'
    { instanceId = pInstanceId_,
      promptId = pPromptId_
    }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
getPromptFile_instanceId :: Lens.Lens' GetPromptFile Prelude.Text
getPromptFile_instanceId = Lens.lens (\GetPromptFile' {instanceId} -> instanceId) (\s@GetPromptFile' {} a -> s {instanceId = a} :: GetPromptFile)

-- | A unique identifier for the prompt.
getPromptFile_promptId :: Lens.Lens' GetPromptFile Prelude.Text
getPromptFile_promptId = Lens.lens (\GetPromptFile' {promptId} -> promptId) (\s@GetPromptFile' {} a -> s {promptId = a} :: GetPromptFile)

instance Core.AWSRequest GetPromptFile where
  type
    AWSResponse GetPromptFile =
      GetPromptFileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPromptFileResponse'
            Prelude.<$> (x Data..?> "PromptPresignedUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPromptFile where
  hashWithSalt _salt GetPromptFile' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` promptId

instance Prelude.NFData GetPromptFile where
  rnf GetPromptFile' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf promptId

instance Data.ToHeaders GetPromptFile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetPromptFile where
  toPath GetPromptFile' {..} =
    Prelude.mconcat
      [ "/prompts/",
        Data.toBS instanceId,
        "/",
        Data.toBS promptId,
        "/file"
      ]

instance Data.ToQuery GetPromptFile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPromptFileResponse' smart constructor.
data GetPromptFileResponse = GetPromptFileResponse'
  { -- | A generated URL to the prompt that can be given to an unauthorized user
    -- so they can access the prompt in S3.
    promptPresignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPromptFileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'promptPresignedUrl', 'getPromptFileResponse_promptPresignedUrl' - A generated URL to the prompt that can be given to an unauthorized user
-- so they can access the prompt in S3.
--
-- 'httpStatus', 'getPromptFileResponse_httpStatus' - The response's http status code.
newGetPromptFileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPromptFileResponse
newGetPromptFileResponse pHttpStatus_ =
  GetPromptFileResponse'
    { promptPresignedUrl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A generated URL to the prompt that can be given to an unauthorized user
-- so they can access the prompt in S3.
getPromptFileResponse_promptPresignedUrl :: Lens.Lens' GetPromptFileResponse (Prelude.Maybe Prelude.Text)
getPromptFileResponse_promptPresignedUrl = Lens.lens (\GetPromptFileResponse' {promptPresignedUrl} -> promptPresignedUrl) (\s@GetPromptFileResponse' {} a -> s {promptPresignedUrl = a} :: GetPromptFileResponse)

-- | The response's http status code.
getPromptFileResponse_httpStatus :: Lens.Lens' GetPromptFileResponse Prelude.Int
getPromptFileResponse_httpStatus = Lens.lens (\GetPromptFileResponse' {httpStatus} -> httpStatus) (\s@GetPromptFileResponse' {} a -> s {httpStatus = a} :: GetPromptFileResponse)

instance Prelude.NFData GetPromptFileResponse where
  rnf GetPromptFileResponse' {..} =
    Prelude.rnf promptPresignedUrl
      `Prelude.seq` Prelude.rnf httpStatus
