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
    describeResourceResponse_email,
    describeResourceResponse_state,
    describeResourceResponse_resourceId,
    describeResourceResponse_disabledDate,
    describeResourceResponse_name,
    describeResourceResponse_type,
    describeResourceResponse_enabledDate,
    describeResourceResponse_bookingOptions,
    describeResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDescribeResource' smart constructor.
data DescribeResource = DescribeResource'
  { -- | The identifier associated with the organization for which the resource
    -- is described.
    organizationId :: Prelude.Text,
    -- | The identifier of the resource to be described.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  DescribeResource
newDescribeResource pOrganizationId_ pResourceId_ =
  DescribeResource'
    { organizationId =
        pOrganizationId_,
      resourceId = pResourceId_
    }

-- | The identifier associated with the organization for which the resource
-- is described.
describeResource_organizationId :: Lens.Lens' DescribeResource Prelude.Text
describeResource_organizationId = Lens.lens (\DescribeResource' {organizationId} -> organizationId) (\s@DescribeResource' {} a -> s {organizationId = a} :: DescribeResource)

-- | The identifier of the resource to be described.
describeResource_resourceId :: Lens.Lens' DescribeResource Prelude.Text
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
            Prelude.<$> (x Core..?> "Email")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "ResourceId")
            Prelude.<*> (x Core..?> "DisabledDate")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Type")
            Prelude.<*> (x Core..?> "EnabledDate")
            Prelude.<*> (x Core..?> "BookingOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResource

instance Prelude.NFData DescribeResource

instance Core.ToHeaders DescribeResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DescribeResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeResource where
  toJSON DescribeResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Core..= organizationId),
            Prelude.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath DescribeResource where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourceResponse' smart constructor.
data DescribeResourceResponse = DescribeResourceResponse'
  { -- | The email of the described resource.
    email :: Prelude.Maybe Prelude.Text,
    -- | The state of the resource: enabled (registered to Amazon WorkMail),
    -- disabled (deregistered or never registered to WorkMail), or deleted.
    state :: Prelude.Maybe EntityState,
    -- | The identifier of the described resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when a resource was disabled from WorkMail, in UNIX
    -- epoch time format.
    disabledDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the described resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the described resource.
    type' :: Prelude.Maybe ResourceType,
    -- | The date and time when a resource was enabled for WorkMail, in UNIX
    -- epoch time format.
    enabledDate :: Prelude.Maybe Core.POSIX,
    -- | The booking options for the described resource.
    bookingOptions :: Prelude.Maybe BookingOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'email', 'describeResourceResponse_email' - The email of the described resource.
--
-- 'state', 'describeResourceResponse_state' - The state of the resource: enabled (registered to Amazon WorkMail),
-- disabled (deregistered or never registered to WorkMail), or deleted.
--
-- 'resourceId', 'describeResourceResponse_resourceId' - The identifier of the described resource.
--
-- 'disabledDate', 'describeResourceResponse_disabledDate' - The date and time when a resource was disabled from WorkMail, in UNIX
-- epoch time format.
--
-- 'name', 'describeResourceResponse_name' - The name of the described resource.
--
-- 'type'', 'describeResourceResponse_type' - The type of the described resource.
--
-- 'enabledDate', 'describeResourceResponse_enabledDate' - The date and time when a resource was enabled for WorkMail, in UNIX
-- epoch time format.
--
-- 'bookingOptions', 'describeResourceResponse_bookingOptions' - The booking options for the described resource.
--
-- 'httpStatus', 'describeResourceResponse_httpStatus' - The response's http status code.
newDescribeResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourceResponse
newDescribeResourceResponse pHttpStatus_ =
  DescribeResourceResponse'
    { email = Prelude.Nothing,
      state = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      disabledDate = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      enabledDate = Prelude.Nothing,
      bookingOptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The email of the described resource.
describeResourceResponse_email :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.Text)
describeResourceResponse_email = Lens.lens (\DescribeResourceResponse' {email} -> email) (\s@DescribeResourceResponse' {} a -> s {email = a} :: DescribeResourceResponse)

-- | The state of the resource: enabled (registered to Amazon WorkMail),
-- disabled (deregistered or never registered to WorkMail), or deleted.
describeResourceResponse_state :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe EntityState)
describeResourceResponse_state = Lens.lens (\DescribeResourceResponse' {state} -> state) (\s@DescribeResourceResponse' {} a -> s {state = a} :: DescribeResourceResponse)

-- | The identifier of the described resource.
describeResourceResponse_resourceId :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.Text)
describeResourceResponse_resourceId = Lens.lens (\DescribeResourceResponse' {resourceId} -> resourceId) (\s@DescribeResourceResponse' {} a -> s {resourceId = a} :: DescribeResourceResponse)

-- | The date and time when a resource was disabled from WorkMail, in UNIX
-- epoch time format.
describeResourceResponse_disabledDate :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.UTCTime)
describeResourceResponse_disabledDate = Lens.lens (\DescribeResourceResponse' {disabledDate} -> disabledDate) (\s@DescribeResourceResponse' {} a -> s {disabledDate = a} :: DescribeResourceResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the described resource.
describeResourceResponse_name :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.Text)
describeResourceResponse_name = Lens.lens (\DescribeResourceResponse' {name} -> name) (\s@DescribeResourceResponse' {} a -> s {name = a} :: DescribeResourceResponse)

-- | The type of the described resource.
describeResourceResponse_type :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe ResourceType)
describeResourceResponse_type = Lens.lens (\DescribeResourceResponse' {type'} -> type') (\s@DescribeResourceResponse' {} a -> s {type' = a} :: DescribeResourceResponse)

-- | The date and time when a resource was enabled for WorkMail, in UNIX
-- epoch time format.
describeResourceResponse_enabledDate :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.UTCTime)
describeResourceResponse_enabledDate = Lens.lens (\DescribeResourceResponse' {enabledDate} -> enabledDate) (\s@DescribeResourceResponse' {} a -> s {enabledDate = a} :: DescribeResourceResponse) Prelude.. Lens.mapping Core._Time

-- | The booking options for the described resource.
describeResourceResponse_bookingOptions :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe BookingOptions)
describeResourceResponse_bookingOptions = Lens.lens (\DescribeResourceResponse' {bookingOptions} -> bookingOptions) (\s@DescribeResourceResponse' {} a -> s {bookingOptions = a} :: DescribeResourceResponse)

-- | The response's http status code.
describeResourceResponse_httpStatus :: Lens.Lens' DescribeResourceResponse Prelude.Int
describeResourceResponse_httpStatus = Lens.lens (\DescribeResourceResponse' {httpStatus} -> httpStatus) (\s@DescribeResourceResponse' {} a -> s {httpStatus = a} :: DescribeResourceResponse)

instance Prelude.NFData DescribeResourceResponse
