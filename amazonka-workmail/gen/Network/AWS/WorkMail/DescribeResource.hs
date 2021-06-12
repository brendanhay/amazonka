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
-- Module      : Network.AWS.WorkMail.DescribeResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data available for the resource.
module Network.AWS.WorkMail.DescribeResource
  ( -- * Creating a Request
    DescribeResource (..),
    newDescribeResource,

    -- * Request Lenses
    describeResource_organizationId,
    describeResource_resourceId,

    -- * Destructuring the Response
    DescribeResourceResponse (..),
    newDescribeResourceResponse,

    -- * Response Lenses
    describeResourceResponse_resourceId,
    describeResourceResponse_enabledDate,
    describeResourceResponse_state,
    describeResourceResponse_name,
    describeResourceResponse_email,
    describeResourceResponse_disabledDate,
    describeResourceResponse_bookingOptions,
    describeResourceResponse_type,
    describeResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDescribeResource' smart constructor.
data DescribeResource = DescribeResource'
  { -- | The identifier associated with the organization for which the resource
    -- is described.
    organizationId :: Core.Text,
    -- | The identifier of the resource to be described.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'describeResource_organizationId' - The identifier associated with the organization for which the resource
-- is described.
--
-- 'resourceId', 'describeResource_resourceId' - The identifier of the resource to be described.
newDescribeResource ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  DescribeResource
newDescribeResource pOrganizationId_ pResourceId_ =
  DescribeResource'
    { organizationId =
        pOrganizationId_,
      resourceId = pResourceId_
    }

-- | The identifier associated with the organization for which the resource
-- is described.
describeResource_organizationId :: Lens.Lens' DescribeResource Core.Text
describeResource_organizationId = Lens.lens (\DescribeResource' {organizationId} -> organizationId) (\s@DescribeResource' {} a -> s {organizationId = a} :: DescribeResource)

-- | The identifier of the resource to be described.
describeResource_resourceId :: Lens.Lens' DescribeResource Core.Text
describeResource_resourceId = Lens.lens (\DescribeResource' {resourceId} -> resourceId) (\s@DescribeResource' {} a -> s {resourceId = a} :: DescribeResource)

instance Core.AWSRequest DescribeResource where
  type
    AWSResponse DescribeResource =
      DescribeResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceResponse'
            Core.<$> (x Core..?> "ResourceId")
            Core.<*> (x Core..?> "EnabledDate")
            Core.<*> (x Core..?> "State")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "Email")
            Core.<*> (x Core..?> "DisabledDate")
            Core.<*> (x Core..?> "BookingOptions")
            Core.<*> (x Core..?> "Type")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeResource

instance Core.NFData DescribeResource

instance Core.ToHeaders DescribeResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DescribeResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeResource where
  toJSON DescribeResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath DescribeResource where
  toPath = Core.const "/"

instance Core.ToQuery DescribeResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeResourceResponse' smart constructor.
data DescribeResourceResponse = DescribeResourceResponse'
  { -- | The identifier of the described resource.
    resourceId :: Core.Maybe Core.Text,
    -- | The date and time when a resource was enabled for WorkMail, in UNIX
    -- epoch time format.
    enabledDate :: Core.Maybe Core.POSIX,
    -- | The state of the resource: enabled (registered to Amazon WorkMail),
    -- disabled (deregistered or never registered to WorkMail), or deleted.
    state :: Core.Maybe EntityState,
    -- | The name of the described resource.
    name :: Core.Maybe Core.Text,
    -- | The email of the described resource.
    email :: Core.Maybe Core.Text,
    -- | The date and time when a resource was disabled from WorkMail, in UNIX
    -- epoch time format.
    disabledDate :: Core.Maybe Core.POSIX,
    -- | The booking options for the described resource.
    bookingOptions :: Core.Maybe BookingOptions,
    -- | The type of the described resource.
    type' :: Core.Maybe ResourceType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeResourceResponse_resourceId' - The identifier of the described resource.
--
-- 'enabledDate', 'describeResourceResponse_enabledDate' - The date and time when a resource was enabled for WorkMail, in UNIX
-- epoch time format.
--
-- 'state', 'describeResourceResponse_state' - The state of the resource: enabled (registered to Amazon WorkMail),
-- disabled (deregistered or never registered to WorkMail), or deleted.
--
-- 'name', 'describeResourceResponse_name' - The name of the described resource.
--
-- 'email', 'describeResourceResponse_email' - The email of the described resource.
--
-- 'disabledDate', 'describeResourceResponse_disabledDate' - The date and time when a resource was disabled from WorkMail, in UNIX
-- epoch time format.
--
-- 'bookingOptions', 'describeResourceResponse_bookingOptions' - The booking options for the described resource.
--
-- 'type'', 'describeResourceResponse_type' - The type of the described resource.
--
-- 'httpStatus', 'describeResourceResponse_httpStatus' - The response's http status code.
newDescribeResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeResourceResponse
newDescribeResourceResponse pHttpStatus_ =
  DescribeResourceResponse'
    { resourceId =
        Core.Nothing,
      enabledDate = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      email = Core.Nothing,
      disabledDate = Core.Nothing,
      bookingOptions = Core.Nothing,
      type' = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the described resource.
describeResourceResponse_resourceId :: Lens.Lens' DescribeResourceResponse (Core.Maybe Core.Text)
describeResourceResponse_resourceId = Lens.lens (\DescribeResourceResponse' {resourceId} -> resourceId) (\s@DescribeResourceResponse' {} a -> s {resourceId = a} :: DescribeResourceResponse)

-- | The date and time when a resource was enabled for WorkMail, in UNIX
-- epoch time format.
describeResourceResponse_enabledDate :: Lens.Lens' DescribeResourceResponse (Core.Maybe Core.UTCTime)
describeResourceResponse_enabledDate = Lens.lens (\DescribeResourceResponse' {enabledDate} -> enabledDate) (\s@DescribeResourceResponse' {} a -> s {enabledDate = a} :: DescribeResourceResponse) Core.. Lens.mapping Core._Time

-- | The state of the resource: enabled (registered to Amazon WorkMail),
-- disabled (deregistered or never registered to WorkMail), or deleted.
describeResourceResponse_state :: Lens.Lens' DescribeResourceResponse (Core.Maybe EntityState)
describeResourceResponse_state = Lens.lens (\DescribeResourceResponse' {state} -> state) (\s@DescribeResourceResponse' {} a -> s {state = a} :: DescribeResourceResponse)

-- | The name of the described resource.
describeResourceResponse_name :: Lens.Lens' DescribeResourceResponse (Core.Maybe Core.Text)
describeResourceResponse_name = Lens.lens (\DescribeResourceResponse' {name} -> name) (\s@DescribeResourceResponse' {} a -> s {name = a} :: DescribeResourceResponse)

-- | The email of the described resource.
describeResourceResponse_email :: Lens.Lens' DescribeResourceResponse (Core.Maybe Core.Text)
describeResourceResponse_email = Lens.lens (\DescribeResourceResponse' {email} -> email) (\s@DescribeResourceResponse' {} a -> s {email = a} :: DescribeResourceResponse)

-- | The date and time when a resource was disabled from WorkMail, in UNIX
-- epoch time format.
describeResourceResponse_disabledDate :: Lens.Lens' DescribeResourceResponse (Core.Maybe Core.UTCTime)
describeResourceResponse_disabledDate = Lens.lens (\DescribeResourceResponse' {disabledDate} -> disabledDate) (\s@DescribeResourceResponse' {} a -> s {disabledDate = a} :: DescribeResourceResponse) Core.. Lens.mapping Core._Time

-- | The booking options for the described resource.
describeResourceResponse_bookingOptions :: Lens.Lens' DescribeResourceResponse (Core.Maybe BookingOptions)
describeResourceResponse_bookingOptions = Lens.lens (\DescribeResourceResponse' {bookingOptions} -> bookingOptions) (\s@DescribeResourceResponse' {} a -> s {bookingOptions = a} :: DescribeResourceResponse)

-- | The type of the described resource.
describeResourceResponse_type :: Lens.Lens' DescribeResourceResponse (Core.Maybe ResourceType)
describeResourceResponse_type = Lens.lens (\DescribeResourceResponse' {type'} -> type') (\s@DescribeResourceResponse' {} a -> s {type' = a} :: DescribeResourceResponse)

-- | The response's http status code.
describeResourceResponse_httpStatus :: Lens.Lens' DescribeResourceResponse Core.Int
describeResourceResponse_httpStatus = Lens.lens (\DescribeResourceResponse' {httpStatus} -> httpStatus) (\s@DescribeResourceResponse' {} a -> s {httpStatus = a} :: DescribeResourceResponse)

instance Core.NFData DescribeResourceResponse
