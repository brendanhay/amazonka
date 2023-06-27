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
-- Module      : Amazonka.Connect.CreatePrompt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a prompt. For more information about prompts, such as supported
-- file types and maximum length, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/prompts.html Create prompts>
-- in the /Amazon Connect Administrator\'s Guide/.
module Amazonka.Connect.CreatePrompt
  ( -- * Creating a Request
    CreatePrompt (..),
    newCreatePrompt,

    -- * Request Lenses
    createPrompt_description,
    createPrompt_tags,
    createPrompt_instanceId,
    createPrompt_name,
    createPrompt_s3Uri,

    -- * Destructuring the Response
    CreatePromptResponse (..),
    newCreatePromptResponse,

    -- * Response Lenses
    createPromptResponse_promptARN,
    createPromptResponse_promptId,
    createPromptResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePrompt' smart constructor.
data CreatePrompt = CreatePrompt'
  { -- | The description of the prompt.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The name of the prompt.
    name :: Prelude.Text,
    -- | The URI for the S3 bucket where the prompt is stored.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePrompt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createPrompt_description' - The description of the prompt.
--
-- 'tags', 'createPrompt_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'instanceId', 'createPrompt_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'name', 'createPrompt_name' - The name of the prompt.
--
-- 's3Uri', 'createPrompt_s3Uri' - The URI for the S3 bucket where the prompt is stored.
newCreatePrompt ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 's3Uri'
  Prelude.Text ->
  CreatePrompt
newCreatePrompt pInstanceId_ pName_ pS3Uri_ =
  CreatePrompt'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      instanceId = pInstanceId_,
      name = pName_,
      s3Uri = pS3Uri_
    }

-- | The description of the prompt.
createPrompt_description :: Lens.Lens' CreatePrompt (Prelude.Maybe Prelude.Text)
createPrompt_description = Lens.lens (\CreatePrompt' {description} -> description) (\s@CreatePrompt' {} a -> s {description = a} :: CreatePrompt)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createPrompt_tags :: Lens.Lens' CreatePrompt (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPrompt_tags = Lens.lens (\CreatePrompt' {tags} -> tags) (\s@CreatePrompt' {} a -> s {tags = a} :: CreatePrompt) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
createPrompt_instanceId :: Lens.Lens' CreatePrompt Prelude.Text
createPrompt_instanceId = Lens.lens (\CreatePrompt' {instanceId} -> instanceId) (\s@CreatePrompt' {} a -> s {instanceId = a} :: CreatePrompt)

-- | The name of the prompt.
createPrompt_name :: Lens.Lens' CreatePrompt Prelude.Text
createPrompt_name = Lens.lens (\CreatePrompt' {name} -> name) (\s@CreatePrompt' {} a -> s {name = a} :: CreatePrompt)

-- | The URI for the S3 bucket where the prompt is stored.
createPrompt_s3Uri :: Lens.Lens' CreatePrompt Prelude.Text
createPrompt_s3Uri = Lens.lens (\CreatePrompt' {s3Uri} -> s3Uri) (\s@CreatePrompt' {} a -> s {s3Uri = a} :: CreatePrompt)

instance Core.AWSRequest CreatePrompt where
  type AWSResponse CreatePrompt = CreatePromptResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePromptResponse'
            Prelude.<$> (x Data..?> "PromptARN")
            Prelude.<*> (x Data..?> "PromptId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePrompt where
  hashWithSalt _salt CreatePrompt' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData CreatePrompt where
  rnf CreatePrompt' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToHeaders CreatePrompt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePrompt where
  toJSON CreatePrompt' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )

instance Data.ToPath CreatePrompt where
  toPath CreatePrompt' {..} =
    Prelude.mconcat ["/prompts/", Data.toBS instanceId]

instance Data.ToQuery CreatePrompt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePromptResponse' smart constructor.
data CreatePromptResponse = CreatePromptResponse'
  { -- | The Amazon Resource Name (ARN) of the prompt.
    promptARN :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the prompt.
    promptId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePromptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'promptARN', 'createPromptResponse_promptARN' - The Amazon Resource Name (ARN) of the prompt.
--
-- 'promptId', 'createPromptResponse_promptId' - A unique identifier for the prompt.
--
-- 'httpStatus', 'createPromptResponse_httpStatus' - The response's http status code.
newCreatePromptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePromptResponse
newCreatePromptResponse pHttpStatus_ =
  CreatePromptResponse'
    { promptARN = Prelude.Nothing,
      promptId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the prompt.
createPromptResponse_promptARN :: Lens.Lens' CreatePromptResponse (Prelude.Maybe Prelude.Text)
createPromptResponse_promptARN = Lens.lens (\CreatePromptResponse' {promptARN} -> promptARN) (\s@CreatePromptResponse' {} a -> s {promptARN = a} :: CreatePromptResponse)

-- | A unique identifier for the prompt.
createPromptResponse_promptId :: Lens.Lens' CreatePromptResponse (Prelude.Maybe Prelude.Text)
createPromptResponse_promptId = Lens.lens (\CreatePromptResponse' {promptId} -> promptId) (\s@CreatePromptResponse' {} a -> s {promptId = a} :: CreatePromptResponse)

-- | The response's http status code.
createPromptResponse_httpStatus :: Lens.Lens' CreatePromptResponse Prelude.Int
createPromptResponse_httpStatus = Lens.lens (\CreatePromptResponse' {httpStatus} -> httpStatus) (\s@CreatePromptResponse' {} a -> s {httpStatus = a} :: CreatePromptResponse)

instance Prelude.NFData CreatePromptResponse where
  rnf CreatePromptResponse' {..} =
    Prelude.rnf promptARN
      `Prelude.seq` Prelude.rnf promptId
      `Prelude.seq` Prelude.rnf httpStatus
