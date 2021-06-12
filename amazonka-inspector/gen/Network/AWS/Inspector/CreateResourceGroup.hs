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
-- Module      : Network.AWS.Inspector.CreateResourceGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource group using the specified set of tags (key and value
-- pairs) that are used to select the EC2 instances to be included in an
-- Amazon Inspector assessment target. The created resource group is then
-- used to create an Amazon Inspector assessment target. For more
-- information, see CreateAssessmentTarget.
module Network.AWS.Inspector.CreateResourceGroup
  ( -- * Creating a Request
    CreateResourceGroup (..),
    newCreateResourceGroup,

    -- * Request Lenses
    createResourceGroup_resourceGroupTags,

    -- * Destructuring the Response
    CreateResourceGroupResponse (..),
    newCreateResourceGroupResponse,

    -- * Response Lenses
    createResourceGroupResponse_httpStatus,
    createResourceGroupResponse_resourceGroupArn,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateResourceGroup' smart constructor.
data CreateResourceGroup = CreateResourceGroup'
  { -- | A collection of keys and an array of possible values,
    -- \'[{\"key\":\"key1\",\"values\":[\"Value1\",\"Value2\"]},{\"key\":\"Key2\",\"values\":[\"Value3\"]}]\'.
    --
    -- For example,\'[{\"key\":\"Name\",\"values\":[\"TestEC2Instance\"]}]\'.
    resourceGroupTags :: Core.NonEmpty ResourceGroupTag
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateResourceGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupTags', 'createResourceGroup_resourceGroupTags' - A collection of keys and an array of possible values,
-- \'[{\"key\":\"key1\",\"values\":[\"Value1\",\"Value2\"]},{\"key\":\"Key2\",\"values\":[\"Value3\"]}]\'.
--
-- For example,\'[{\"key\":\"Name\",\"values\":[\"TestEC2Instance\"]}]\'.
newCreateResourceGroup ::
  -- | 'resourceGroupTags'
  Core.NonEmpty ResourceGroupTag ->
  CreateResourceGroup
newCreateResourceGroup pResourceGroupTags_ =
  CreateResourceGroup'
    { resourceGroupTags =
        Lens._Coerce Lens.# pResourceGroupTags_
    }

-- | A collection of keys and an array of possible values,
-- \'[{\"key\":\"key1\",\"values\":[\"Value1\",\"Value2\"]},{\"key\":\"Key2\",\"values\":[\"Value3\"]}]\'.
--
-- For example,\'[{\"key\":\"Name\",\"values\":[\"TestEC2Instance\"]}]\'.
createResourceGroup_resourceGroupTags :: Lens.Lens' CreateResourceGroup (Core.NonEmpty ResourceGroupTag)
createResourceGroup_resourceGroupTags = Lens.lens (\CreateResourceGroup' {resourceGroupTags} -> resourceGroupTags) (\s@CreateResourceGroup' {} a -> s {resourceGroupTags = a} :: CreateResourceGroup) Core.. Lens._Coerce

instance Core.AWSRequest CreateResourceGroup where
  type
    AWSResponse CreateResourceGroup =
      CreateResourceGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "resourceGroupArn")
      )

instance Core.Hashable CreateResourceGroup

instance Core.NFData CreateResourceGroup

instance Core.ToHeaders CreateResourceGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.CreateResourceGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateResourceGroup where
  toJSON CreateResourceGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("resourceGroupTags" Core..= resourceGroupTags)
          ]
      )

instance Core.ToPath CreateResourceGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateResourceGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateResourceGroupResponse' smart constructor.
data CreateResourceGroupResponse = CreateResourceGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ARN that specifies the resource group that is created.
    resourceGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateResourceGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createResourceGroupResponse_httpStatus' - The response's http status code.
--
-- 'resourceGroupArn', 'createResourceGroupResponse_resourceGroupArn' - The ARN that specifies the resource group that is created.
newCreateResourceGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'resourceGroupArn'
  Core.Text ->
  CreateResourceGroupResponse
newCreateResourceGroupResponse
  pHttpStatus_
  pResourceGroupArn_ =
    CreateResourceGroupResponse'
      { httpStatus =
          pHttpStatus_,
        resourceGroupArn = pResourceGroupArn_
      }

-- | The response's http status code.
createResourceGroupResponse_httpStatus :: Lens.Lens' CreateResourceGroupResponse Core.Int
createResourceGroupResponse_httpStatus = Lens.lens (\CreateResourceGroupResponse' {httpStatus} -> httpStatus) (\s@CreateResourceGroupResponse' {} a -> s {httpStatus = a} :: CreateResourceGroupResponse)

-- | The ARN that specifies the resource group that is created.
createResourceGroupResponse_resourceGroupArn :: Lens.Lens' CreateResourceGroupResponse Core.Text
createResourceGroupResponse_resourceGroupArn = Lens.lens (\CreateResourceGroupResponse' {resourceGroupArn} -> resourceGroupArn) (\s@CreateResourceGroupResponse' {} a -> s {resourceGroupArn = a} :: CreateResourceGroupResponse)

instance Core.NFData CreateResourceGroupResponse
