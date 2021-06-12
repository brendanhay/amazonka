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
-- Module      : Network.AWS.Batch.DeregisterJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an AWS Batch job definition. Job definitions are permanently
-- deleted after 180 days.
module Network.AWS.Batch.DeregisterJobDefinition
  ( -- * Creating a Request
    DeregisterJobDefinition (..),
    newDeregisterJobDefinition,

    -- * Request Lenses
    deregisterJobDefinition_jobDefinition,

    -- * Destructuring the Response
    DeregisterJobDefinitionResponse (..),
    newDeregisterJobDefinitionResponse,

    -- * Response Lenses
    deregisterJobDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterJobDefinition' smart constructor.
data DeregisterJobDefinition = DeregisterJobDefinition'
  { -- | The name and revision (@name:revision@) or full Amazon Resource Name
    -- (ARN) of the job definition to deregister.
    jobDefinition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinition', 'deregisterJobDefinition_jobDefinition' - The name and revision (@name:revision@) or full Amazon Resource Name
-- (ARN) of the job definition to deregister.
newDeregisterJobDefinition ::
  -- | 'jobDefinition'
  Core.Text ->
  DeregisterJobDefinition
newDeregisterJobDefinition pJobDefinition_ =
  DeregisterJobDefinition'
    { jobDefinition =
        pJobDefinition_
    }

-- | The name and revision (@name:revision@) or full Amazon Resource Name
-- (ARN) of the job definition to deregister.
deregisterJobDefinition_jobDefinition :: Lens.Lens' DeregisterJobDefinition Core.Text
deregisterJobDefinition_jobDefinition = Lens.lens (\DeregisterJobDefinition' {jobDefinition} -> jobDefinition) (\s@DeregisterJobDefinition' {} a -> s {jobDefinition = a} :: DeregisterJobDefinition)

instance Core.AWSRequest DeregisterJobDefinition where
  type
    AWSResponse DeregisterJobDefinition =
      DeregisterJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterJobDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterJobDefinition

instance Core.NFData DeregisterJobDefinition

instance Core.ToHeaders DeregisterJobDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterJobDefinition where
  toJSON DeregisterJobDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("jobDefinition" Core..= jobDefinition)]
      )

instance Core.ToPath DeregisterJobDefinition where
  toPath = Core.const "/v1/deregisterjobdefinition"

instance Core.ToQuery DeregisterJobDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterJobDefinitionResponse' smart constructor.
data DeregisterJobDefinitionResponse = DeregisterJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterJobDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterJobDefinitionResponse_httpStatus' - The response's http status code.
newDeregisterJobDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeregisterJobDefinitionResponse
newDeregisterJobDefinitionResponse pHttpStatus_ =
  DeregisterJobDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterJobDefinitionResponse_httpStatus :: Lens.Lens' DeregisterJobDefinitionResponse Core.Int
deregisterJobDefinitionResponse_httpStatus = Lens.lens (\DeregisterJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeregisterJobDefinitionResponse' {} a -> s {httpStatus = a} :: DeregisterJobDefinitionResponse)

instance Core.NFData DeregisterJobDefinitionResponse
