{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterJobDefinition' smart constructor.
data DeregisterJobDefinition = DeregisterJobDefinition'
  { -- | The name and revision (@name:revision@) or full Amazon Resource Name
    -- (ARN) of the job definition to deregister.
    jobDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeregisterJobDefinition
newDeregisterJobDefinition pJobDefinition_ =
  DeregisterJobDefinition'
    { jobDefinition =
        pJobDefinition_
    }

-- | The name and revision (@name:revision@) or full Amazon Resource Name
-- (ARN) of the job definition to deregister.
deregisterJobDefinition_jobDefinition :: Lens.Lens' DeregisterJobDefinition Prelude.Text
deregisterJobDefinition_jobDefinition = Lens.lens (\DeregisterJobDefinition' {jobDefinition} -> jobDefinition) (\s@DeregisterJobDefinition' {} a -> s {jobDefinition = a} :: DeregisterJobDefinition)

instance Prelude.AWSRequest DeregisterJobDefinition where
  type
    Rs DeregisterJobDefinition =
      DeregisterJobDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterJobDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterJobDefinition

instance Prelude.NFData DeregisterJobDefinition

instance Prelude.ToHeaders DeregisterJobDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeregisterJobDefinition where
  toJSON DeregisterJobDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("jobDefinition" Prelude..= jobDefinition)
          ]
      )

instance Prelude.ToPath DeregisterJobDefinition where
  toPath = Prelude.const "/v1/deregisterjobdefinition"

instance Prelude.ToQuery DeregisterJobDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterJobDefinitionResponse' smart constructor.
data DeregisterJobDefinitionResponse = DeregisterJobDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeregisterJobDefinitionResponse
newDeregisterJobDefinitionResponse pHttpStatus_ =
  DeregisterJobDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterJobDefinitionResponse_httpStatus :: Lens.Lens' DeregisterJobDefinitionResponse Prelude.Int
deregisterJobDefinitionResponse_httpStatus = Lens.lens (\DeregisterJobDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeregisterJobDefinitionResponse' {} a -> s {httpStatus = a} :: DeregisterJobDefinitionResponse)

instance
  Prelude.NFData
    DeregisterJobDefinitionResponse
