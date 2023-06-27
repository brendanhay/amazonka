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
-- Module      : Amazonka.Connect.UpdatePrompt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a prompt.
module Amazonka.Connect.UpdatePrompt
  ( -- * Creating a Request
    UpdatePrompt (..),
    newUpdatePrompt,

    -- * Request Lenses
    updatePrompt_description,
    updatePrompt_name,
    updatePrompt_s3Uri,
    updatePrompt_instanceId,
    updatePrompt_promptId,

    -- * Destructuring the Response
    UpdatePromptResponse (..),
    newUpdatePromptResponse,

    -- * Response Lenses
    updatePromptResponse_promptARN,
    updatePromptResponse_promptId,
    updatePromptResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePrompt' smart constructor.
data UpdatePrompt = UpdatePrompt'
  { -- | A description of the prompt.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the prompt.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URI for the S3 bucket where the prompt is stored.
    s3Uri :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the prompt.
    promptId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePrompt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updatePrompt_description' - A description of the prompt.
--
-- 'name', 'updatePrompt_name' - The name of the prompt.
--
-- 's3Uri', 'updatePrompt_s3Uri' - The URI for the S3 bucket where the prompt is stored.
--
-- 'instanceId', 'updatePrompt_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'promptId', 'updatePrompt_promptId' - A unique identifier for the prompt.
newUpdatePrompt ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'promptId'
  Prelude.Text ->
  UpdatePrompt
newUpdatePrompt pInstanceId_ pPromptId_ =
  UpdatePrompt'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      s3Uri = Prelude.Nothing,
      instanceId = pInstanceId_,
      promptId = pPromptId_
    }

-- | A description of the prompt.
updatePrompt_description :: Lens.Lens' UpdatePrompt (Prelude.Maybe Prelude.Text)
updatePrompt_description = Lens.lens (\UpdatePrompt' {description} -> description) (\s@UpdatePrompt' {} a -> s {description = a} :: UpdatePrompt)

-- | The name of the prompt.
updatePrompt_name :: Lens.Lens' UpdatePrompt (Prelude.Maybe Prelude.Text)
updatePrompt_name = Lens.lens (\UpdatePrompt' {name} -> name) (\s@UpdatePrompt' {} a -> s {name = a} :: UpdatePrompt)

-- | The URI for the S3 bucket where the prompt is stored.
updatePrompt_s3Uri :: Lens.Lens' UpdatePrompt (Prelude.Maybe Prelude.Text)
updatePrompt_s3Uri = Lens.lens (\UpdatePrompt' {s3Uri} -> s3Uri) (\s@UpdatePrompt' {} a -> s {s3Uri = a} :: UpdatePrompt)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
updatePrompt_instanceId :: Lens.Lens' UpdatePrompt Prelude.Text
updatePrompt_instanceId = Lens.lens (\UpdatePrompt' {instanceId} -> instanceId) (\s@UpdatePrompt' {} a -> s {instanceId = a} :: UpdatePrompt)

-- | A unique identifier for the prompt.
updatePrompt_promptId :: Lens.Lens' UpdatePrompt Prelude.Text
updatePrompt_promptId = Lens.lens (\UpdatePrompt' {promptId} -> promptId) (\s@UpdatePrompt' {} a -> s {promptId = a} :: UpdatePrompt)

instance Core.AWSRequest UpdatePrompt where
  type AWSResponse UpdatePrompt = UpdatePromptResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePromptResponse'
            Prelude.<$> (x Data..?> "PromptARN")
            Prelude.<*> (x Data..?> "PromptId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePrompt where
  hashWithSalt _salt UpdatePrompt' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3Uri
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` promptId

instance Prelude.NFData UpdatePrompt where
  rnf UpdatePrompt' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf s3Uri
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf promptId

instance Data.ToHeaders UpdatePrompt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePrompt where
  toJSON UpdatePrompt' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("S3Uri" Data..=) Prelude.<$> s3Uri
          ]
      )

instance Data.ToPath UpdatePrompt where
  toPath UpdatePrompt' {..} =
    Prelude.mconcat
      [ "/prompts/",
        Data.toBS instanceId,
        "/",
        Data.toBS promptId
      ]

instance Data.ToQuery UpdatePrompt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePromptResponse' smart constructor.
data UpdatePromptResponse = UpdatePromptResponse'
  { -- | The Amazon Resource Name (ARN) of the prompt.
    promptARN :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the prompt.
    promptId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePromptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'promptARN', 'updatePromptResponse_promptARN' - The Amazon Resource Name (ARN) of the prompt.
--
-- 'promptId', 'updatePromptResponse_promptId' - A unique identifier for the prompt.
--
-- 'httpStatus', 'updatePromptResponse_httpStatus' - The response's http status code.
newUpdatePromptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePromptResponse
newUpdatePromptResponse pHttpStatus_ =
  UpdatePromptResponse'
    { promptARN = Prelude.Nothing,
      promptId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the prompt.
updatePromptResponse_promptARN :: Lens.Lens' UpdatePromptResponse (Prelude.Maybe Prelude.Text)
updatePromptResponse_promptARN = Lens.lens (\UpdatePromptResponse' {promptARN} -> promptARN) (\s@UpdatePromptResponse' {} a -> s {promptARN = a} :: UpdatePromptResponse)

-- | A unique identifier for the prompt.
updatePromptResponse_promptId :: Lens.Lens' UpdatePromptResponse (Prelude.Maybe Prelude.Text)
updatePromptResponse_promptId = Lens.lens (\UpdatePromptResponse' {promptId} -> promptId) (\s@UpdatePromptResponse' {} a -> s {promptId = a} :: UpdatePromptResponse)

-- | The response's http status code.
updatePromptResponse_httpStatus :: Lens.Lens' UpdatePromptResponse Prelude.Int
updatePromptResponse_httpStatus = Lens.lens (\UpdatePromptResponse' {httpStatus} -> httpStatus) (\s@UpdatePromptResponse' {} a -> s {httpStatus = a} :: UpdatePromptResponse)

instance Prelude.NFData UpdatePromptResponse where
  rnf UpdatePromptResponse' {..} =
    Prelude.rnf promptARN
      `Prelude.seq` Prelude.rnf promptId
      `Prelude.seq` Prelude.rnf httpStatus
