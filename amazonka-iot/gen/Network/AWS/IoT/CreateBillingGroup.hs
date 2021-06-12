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
-- Module      : Network.AWS.IoT.CreateBillingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a billing group.
module Network.AWS.IoT.CreateBillingGroup
  ( -- * Creating a Request
    CreateBillingGroup (..),
    newCreateBillingGroup,

    -- * Request Lenses
    createBillingGroup_billingGroupProperties,
    createBillingGroup_tags,
    createBillingGroup_billingGroupName,

    -- * Destructuring the Response
    CreateBillingGroupResponse (..),
    newCreateBillingGroupResponse,

    -- * Response Lenses
    createBillingGroupResponse_billingGroupArn,
    createBillingGroupResponse_billingGroupId,
    createBillingGroupResponse_billingGroupName,
    createBillingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBillingGroup' smart constructor.
data CreateBillingGroup = CreateBillingGroup'
  { -- | The properties of the billing group.
    billingGroupProperties :: Core.Maybe BillingGroupProperties,
    -- | Metadata which can be used to manage the billing group.
    tags :: Core.Maybe [Tag],
    -- | The name you wish to give to the billing group.
    billingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupProperties', 'createBillingGroup_billingGroupProperties' - The properties of the billing group.
--
-- 'tags', 'createBillingGroup_tags' - Metadata which can be used to manage the billing group.
--
-- 'billingGroupName', 'createBillingGroup_billingGroupName' - The name you wish to give to the billing group.
newCreateBillingGroup ::
  -- | 'billingGroupName'
  Core.Text ->
  CreateBillingGroup
newCreateBillingGroup pBillingGroupName_ =
  CreateBillingGroup'
    { billingGroupProperties =
        Core.Nothing,
      tags = Core.Nothing,
      billingGroupName = pBillingGroupName_
    }

-- | The properties of the billing group.
createBillingGroup_billingGroupProperties :: Lens.Lens' CreateBillingGroup (Core.Maybe BillingGroupProperties)
createBillingGroup_billingGroupProperties = Lens.lens (\CreateBillingGroup' {billingGroupProperties} -> billingGroupProperties) (\s@CreateBillingGroup' {} a -> s {billingGroupProperties = a} :: CreateBillingGroup)

-- | Metadata which can be used to manage the billing group.
createBillingGroup_tags :: Lens.Lens' CreateBillingGroup (Core.Maybe [Tag])
createBillingGroup_tags = Lens.lens (\CreateBillingGroup' {tags} -> tags) (\s@CreateBillingGroup' {} a -> s {tags = a} :: CreateBillingGroup) Core.. Lens.mapping Lens._Coerce

-- | The name you wish to give to the billing group.
createBillingGroup_billingGroupName :: Lens.Lens' CreateBillingGroup Core.Text
createBillingGroup_billingGroupName = Lens.lens (\CreateBillingGroup' {billingGroupName} -> billingGroupName) (\s@CreateBillingGroup' {} a -> s {billingGroupName = a} :: CreateBillingGroup)

instance Core.AWSRequest CreateBillingGroup where
  type
    AWSResponse CreateBillingGroup =
      CreateBillingGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBillingGroupResponse'
            Core.<$> (x Core..?> "billingGroupArn")
            Core.<*> (x Core..?> "billingGroupId")
            Core.<*> (x Core..?> "billingGroupName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateBillingGroup

instance Core.NFData CreateBillingGroup

instance Core.ToHeaders CreateBillingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateBillingGroup where
  toJSON CreateBillingGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("billingGroupProperties" Core..=)
              Core.<$> billingGroupProperties,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateBillingGroup where
  toPath CreateBillingGroup' {..} =
    Core.mconcat
      ["/billing-groups/", Core.toBS billingGroupName]

instance Core.ToQuery CreateBillingGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateBillingGroupResponse' smart constructor.
data CreateBillingGroupResponse = CreateBillingGroupResponse'
  { -- | The ARN of the billing group.
    billingGroupArn :: Core.Maybe Core.Text,
    -- | The ID of the billing group.
    billingGroupId :: Core.Maybe Core.Text,
    -- | The name you gave to the billing group.
    billingGroupName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupArn', 'createBillingGroupResponse_billingGroupArn' - The ARN of the billing group.
--
-- 'billingGroupId', 'createBillingGroupResponse_billingGroupId' - The ID of the billing group.
--
-- 'billingGroupName', 'createBillingGroupResponse_billingGroupName' - The name you gave to the billing group.
--
-- 'httpStatus', 'createBillingGroupResponse_httpStatus' - The response's http status code.
newCreateBillingGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateBillingGroupResponse
newCreateBillingGroupResponse pHttpStatus_ =
  CreateBillingGroupResponse'
    { billingGroupArn =
        Core.Nothing,
      billingGroupId = Core.Nothing,
      billingGroupName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the billing group.
createBillingGroupResponse_billingGroupArn :: Lens.Lens' CreateBillingGroupResponse (Core.Maybe Core.Text)
createBillingGroupResponse_billingGroupArn = Lens.lens (\CreateBillingGroupResponse' {billingGroupArn} -> billingGroupArn) (\s@CreateBillingGroupResponse' {} a -> s {billingGroupArn = a} :: CreateBillingGroupResponse)

-- | The ID of the billing group.
createBillingGroupResponse_billingGroupId :: Lens.Lens' CreateBillingGroupResponse (Core.Maybe Core.Text)
createBillingGroupResponse_billingGroupId = Lens.lens (\CreateBillingGroupResponse' {billingGroupId} -> billingGroupId) (\s@CreateBillingGroupResponse' {} a -> s {billingGroupId = a} :: CreateBillingGroupResponse)

-- | The name you gave to the billing group.
createBillingGroupResponse_billingGroupName :: Lens.Lens' CreateBillingGroupResponse (Core.Maybe Core.Text)
createBillingGroupResponse_billingGroupName = Lens.lens (\CreateBillingGroupResponse' {billingGroupName} -> billingGroupName) (\s@CreateBillingGroupResponse' {} a -> s {billingGroupName = a} :: CreateBillingGroupResponse)

-- | The response's http status code.
createBillingGroupResponse_httpStatus :: Lens.Lens' CreateBillingGroupResponse Core.Int
createBillingGroupResponse_httpStatus = Lens.lens (\CreateBillingGroupResponse' {httpStatus} -> httpStatus) (\s@CreateBillingGroupResponse' {} a -> s {httpStatus = a} :: CreateBillingGroupResponse)

instance Core.NFData CreateBillingGroupResponse
