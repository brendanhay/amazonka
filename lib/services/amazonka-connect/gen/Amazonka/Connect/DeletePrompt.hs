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
-- Module      : Amazonka.Connect.DeletePrompt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a prompt.
module Amazonka.Connect.DeletePrompt
  ( -- * Creating a Request
    DeletePrompt (..),
    newDeletePrompt,

    -- * Request Lenses
    deletePrompt_instanceId,
    deletePrompt_promptId,

    -- * Destructuring the Response
    DeletePromptResponse (..),
    newDeletePromptResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePrompt' smart constructor.
data DeletePrompt = DeletePrompt'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the prompt.
    promptId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePrompt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deletePrompt_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'promptId', 'deletePrompt_promptId' - A unique identifier for the prompt.
newDeletePrompt ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'promptId'
  Prelude.Text ->
  DeletePrompt
newDeletePrompt pInstanceId_ pPromptId_ =
  DeletePrompt'
    { instanceId = pInstanceId_,
      promptId = pPromptId_
    }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
deletePrompt_instanceId :: Lens.Lens' DeletePrompt Prelude.Text
deletePrompt_instanceId = Lens.lens (\DeletePrompt' {instanceId} -> instanceId) (\s@DeletePrompt' {} a -> s {instanceId = a} :: DeletePrompt)

-- | A unique identifier for the prompt.
deletePrompt_promptId :: Lens.Lens' DeletePrompt Prelude.Text
deletePrompt_promptId = Lens.lens (\DeletePrompt' {promptId} -> promptId) (\s@DeletePrompt' {} a -> s {promptId = a} :: DeletePrompt)

instance Core.AWSRequest DeletePrompt where
  type AWSResponse DeletePrompt = DeletePromptResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeletePromptResponse'

instance Prelude.Hashable DeletePrompt where
  hashWithSalt _salt DeletePrompt' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` promptId

instance Prelude.NFData DeletePrompt where
  rnf DeletePrompt' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf promptId

instance Data.ToHeaders DeletePrompt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePrompt where
  toPath DeletePrompt' {..} =
    Prelude.mconcat
      [ "/prompts/",
        Data.toBS instanceId,
        "/",
        Data.toBS promptId
      ]

instance Data.ToQuery DeletePrompt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePromptResponse' smart constructor.
data DeletePromptResponse = DeletePromptResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePromptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePromptResponse ::
  DeletePromptResponse
newDeletePromptResponse = DeletePromptResponse'

instance Prelude.NFData DeletePromptResponse where
  rnf _ = ()
