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
-- Module      : Network.AWS.DAX.CreateParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new parameter group. A parameter group is a collection of
-- parameters that you apply to all of the nodes in a DAX cluster.
module Network.AWS.DAX.CreateParameterGroup
  ( -- * Creating a Request
    CreateParameterGroup (..),
    newCreateParameterGroup,

    -- * Request Lenses
    createParameterGroup_description,
    createParameterGroup_parameterGroupName,

    -- * Destructuring the Response
    CreateParameterGroupResponse (..),
    newCreateParameterGroupResponse,

    -- * Response Lenses
    createParameterGroupResponse_parameterGroup,
    createParameterGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateParameterGroup' smart constructor.
data CreateParameterGroup = CreateParameterGroup'
  { -- | A description of the parameter group.
    description :: Core.Maybe Core.Text,
    -- | The name of the parameter group to apply to all of the clusters in this
    -- replication group.
    parameterGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createParameterGroup_description' - A description of the parameter group.
--
-- 'parameterGroupName', 'createParameterGroup_parameterGroupName' - The name of the parameter group to apply to all of the clusters in this
-- replication group.
newCreateParameterGroup ::
  -- | 'parameterGroupName'
  Core.Text ->
  CreateParameterGroup
newCreateParameterGroup pParameterGroupName_ =
  CreateParameterGroup'
    { description = Core.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | A description of the parameter group.
createParameterGroup_description :: Lens.Lens' CreateParameterGroup (Core.Maybe Core.Text)
createParameterGroup_description = Lens.lens (\CreateParameterGroup' {description} -> description) (\s@CreateParameterGroup' {} a -> s {description = a} :: CreateParameterGroup)

-- | The name of the parameter group to apply to all of the clusters in this
-- replication group.
createParameterGroup_parameterGroupName :: Lens.Lens' CreateParameterGroup Core.Text
createParameterGroup_parameterGroupName = Lens.lens (\CreateParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@CreateParameterGroup' {} a -> s {parameterGroupName = a} :: CreateParameterGroup)

instance Core.AWSRequest CreateParameterGroup where
  type
    AWSResponse CreateParameterGroup =
      CreateParameterGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateParameterGroupResponse'
            Core.<$> (x Core..?> "ParameterGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateParameterGroup

instance Core.NFData CreateParameterGroup

instance Core.ToHeaders CreateParameterGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.CreateParameterGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateParameterGroup where
  toJSON CreateParameterGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            Core.Just
              ("ParameterGroupName" Core..= parameterGroupName)
          ]
      )

instance Core.ToPath CreateParameterGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateParameterGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateParameterGroupResponse' smart constructor.
data CreateParameterGroupResponse = CreateParameterGroupResponse'
  { -- | Represents the output of a /CreateParameterGroup/ action.
    parameterGroup :: Core.Maybe ParameterGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroup', 'createParameterGroupResponse_parameterGroup' - Represents the output of a /CreateParameterGroup/ action.
--
-- 'httpStatus', 'createParameterGroupResponse_httpStatus' - The response's http status code.
newCreateParameterGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateParameterGroupResponse
newCreateParameterGroupResponse pHttpStatus_ =
  CreateParameterGroupResponse'
    { parameterGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the output of a /CreateParameterGroup/ action.
createParameterGroupResponse_parameterGroup :: Lens.Lens' CreateParameterGroupResponse (Core.Maybe ParameterGroup)
createParameterGroupResponse_parameterGroup = Lens.lens (\CreateParameterGroupResponse' {parameterGroup} -> parameterGroup) (\s@CreateParameterGroupResponse' {} a -> s {parameterGroup = a} :: CreateParameterGroupResponse)

-- | The response's http status code.
createParameterGroupResponse_httpStatus :: Lens.Lens' CreateParameterGroupResponse Core.Int
createParameterGroupResponse_httpStatus = Lens.lens (\CreateParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CreateParameterGroupResponse' {} a -> s {httpStatus = a} :: CreateParameterGroupResponse)

instance Core.NFData CreateParameterGroupResponse
