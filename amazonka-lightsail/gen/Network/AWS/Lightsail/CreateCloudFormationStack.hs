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
-- Module      : Network.AWS.Lightsail.CreateCloudFormationStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation stack, which creates a new Amazon EC2
-- instance from an exported Amazon Lightsail snapshot. This operation
-- results in a CloudFormation stack record that can be used to track the
-- AWS CloudFormation stack created. Use the
-- @get cloud formation stack records@ operation to get a list of the
-- CloudFormation stacks created.
--
-- Wait until after your new Amazon EC2 instance is created before running
-- the @create cloud formation stack@ operation again with the same export
-- snapshot record.
module Network.AWS.Lightsail.CreateCloudFormationStack
  ( -- * Creating a Request
    CreateCloudFormationStack (..),
    newCreateCloudFormationStack,

    -- * Request Lenses
    createCloudFormationStack_instances,

    -- * Destructuring the Response
    CreateCloudFormationStackResponse (..),
    newCreateCloudFormationStackResponse,

    -- * Response Lenses
    createCloudFormationStackResponse_operations,
    createCloudFormationStackResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCloudFormationStack' smart constructor.
data CreateCloudFormationStack = CreateCloudFormationStack'
  { -- | An array of parameters that will be used to create the new Amazon EC2
    -- instance. You can only pass one instance entry at a time in this array.
    -- You will get an invalid parameter error if you pass more than one
    -- instance entry in this array.
    instances :: [InstanceEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCloudFormationStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'createCloudFormationStack_instances' - An array of parameters that will be used to create the new Amazon EC2
-- instance. You can only pass one instance entry at a time in this array.
-- You will get an invalid parameter error if you pass more than one
-- instance entry in this array.
newCreateCloudFormationStack ::
  CreateCloudFormationStack
newCreateCloudFormationStack =
  CreateCloudFormationStack'
    { instances =
        Prelude.mempty
    }

-- | An array of parameters that will be used to create the new Amazon EC2
-- instance. You can only pass one instance entry at a time in this array.
-- You will get an invalid parameter error if you pass more than one
-- instance entry in this array.
createCloudFormationStack_instances :: Lens.Lens' CreateCloudFormationStack [InstanceEntry]
createCloudFormationStack_instances = Lens.lens (\CreateCloudFormationStack' {instances} -> instances) (\s@CreateCloudFormationStack' {} a -> s {instances = a} :: CreateCloudFormationStack) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateCloudFormationStack where
  type
    AWSResponse CreateCloudFormationStack =
      CreateCloudFormationStackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCloudFormationStackResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCloudFormationStack

instance Prelude.NFData CreateCloudFormationStack

instance Core.ToHeaders CreateCloudFormationStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateCloudFormationStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCloudFormationStack where
  toJSON CreateCloudFormationStack' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("instances" Core..= instances)]
      )

instance Core.ToPath CreateCloudFormationStack where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCloudFormationStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCloudFormationStackResponse' smart constructor.
data CreateCloudFormationStackResponse = CreateCloudFormationStackResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCloudFormationStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createCloudFormationStackResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createCloudFormationStackResponse_httpStatus' - The response's http status code.
newCreateCloudFormationStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCloudFormationStackResponse
newCreateCloudFormationStackResponse pHttpStatus_ =
  CreateCloudFormationStackResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createCloudFormationStackResponse_operations :: Lens.Lens' CreateCloudFormationStackResponse (Prelude.Maybe [Operation])
createCloudFormationStackResponse_operations = Lens.lens (\CreateCloudFormationStackResponse' {operations} -> operations) (\s@CreateCloudFormationStackResponse' {} a -> s {operations = a} :: CreateCloudFormationStackResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createCloudFormationStackResponse_httpStatus :: Lens.Lens' CreateCloudFormationStackResponse Prelude.Int
createCloudFormationStackResponse_httpStatus = Lens.lens (\CreateCloudFormationStackResponse' {httpStatus} -> httpStatus) (\s@CreateCloudFormationStackResponse' {} a -> s {httpStatus = a} :: CreateCloudFormationStackResponse)

instance
  Prelude.NFData
    CreateCloudFormationStackResponse
