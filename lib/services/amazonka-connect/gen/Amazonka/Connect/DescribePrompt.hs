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
-- Module      : Amazonka.Connect.DescribePrompt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the prompt.
module Amazonka.Connect.DescribePrompt
  ( -- * Creating a Request
    DescribePrompt (..),
    newDescribePrompt,

    -- * Request Lenses
    describePrompt_instanceId,
    describePrompt_promptId,

    -- * Destructuring the Response
    DescribePromptResponse (..),
    newDescribePromptResponse,

    -- * Response Lenses
    describePromptResponse_prompt,
    describePromptResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePrompt' smart constructor.
data DescribePrompt = DescribePrompt'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the prompt.
    promptId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePrompt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describePrompt_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'promptId', 'describePrompt_promptId' - A unique identifier for the prompt.
newDescribePrompt ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'promptId'
  Prelude.Text ->
  DescribePrompt
newDescribePrompt pInstanceId_ pPromptId_ =
  DescribePrompt'
    { instanceId = pInstanceId_,
      promptId = pPromptId_
    }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
describePrompt_instanceId :: Lens.Lens' DescribePrompt Prelude.Text
describePrompt_instanceId = Lens.lens (\DescribePrompt' {instanceId} -> instanceId) (\s@DescribePrompt' {} a -> s {instanceId = a} :: DescribePrompt)

-- | A unique identifier for the prompt.
describePrompt_promptId :: Lens.Lens' DescribePrompt Prelude.Text
describePrompt_promptId = Lens.lens (\DescribePrompt' {promptId} -> promptId) (\s@DescribePrompt' {} a -> s {promptId = a} :: DescribePrompt)

instance Core.AWSRequest DescribePrompt where
  type
    AWSResponse DescribePrompt =
      DescribePromptResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePromptResponse'
            Prelude.<$> (x Data..?> "Prompt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePrompt where
  hashWithSalt _salt DescribePrompt' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` promptId

instance Prelude.NFData DescribePrompt where
  rnf DescribePrompt' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf promptId

instance Data.ToHeaders DescribePrompt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribePrompt where
  toPath DescribePrompt' {..} =
    Prelude.mconcat
      [ "/prompts/",
        Data.toBS instanceId,
        "/",
        Data.toBS promptId
      ]

instance Data.ToQuery DescribePrompt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePromptResponse' smart constructor.
data DescribePromptResponse = DescribePromptResponse'
  { -- | Information about the prompt.
    prompt :: Prelude.Maybe Prompt,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePromptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prompt', 'describePromptResponse_prompt' - Information about the prompt.
--
-- 'httpStatus', 'describePromptResponse_httpStatus' - The response's http status code.
newDescribePromptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePromptResponse
newDescribePromptResponse pHttpStatus_ =
  DescribePromptResponse'
    { prompt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the prompt.
describePromptResponse_prompt :: Lens.Lens' DescribePromptResponse (Prelude.Maybe Prompt)
describePromptResponse_prompt = Lens.lens (\DescribePromptResponse' {prompt} -> prompt) (\s@DescribePromptResponse' {} a -> s {prompt = a} :: DescribePromptResponse)

-- | The response's http status code.
describePromptResponse_httpStatus :: Lens.Lens' DescribePromptResponse Prelude.Int
describePromptResponse_httpStatus = Lens.lens (\DescribePromptResponse' {httpStatus} -> httpStatus) (\s@DescribePromptResponse' {} a -> s {httpStatus = a} :: DescribePromptResponse)

instance Prelude.NFData DescribePromptResponse where
  rnf DescribePromptResponse' {..} =
    Prelude.rnf prompt
      `Prelude.seq` Prelude.rnf httpStatus
