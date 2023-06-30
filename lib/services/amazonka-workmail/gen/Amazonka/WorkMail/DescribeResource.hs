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
-- Module      : Amazonka.WorkMail.DescribeResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data available for the resource.
module Amazonka.WorkMail.DescribeResource
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
    describeResourceResponse_bookingOptions,
    describeResourceResponse_disabledDate,
    describeResourceResponse_email,
    describeResourceResponse_enabledDate,
    describeResourceResponse_name,
    describeResourceResponse_resourceId,
    describeResourceResponse_state,
    describeResourceResponse_type,
    describeResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceResponse'
            Prelude.<$> (x Data..?> "BookingOptions")
            Prelude.<*> (x Data..?> "DisabledDate")
            Prelude.<*> (x Data..?> "Email")
            Prelude.<*> (x Data..?> "EnabledDate")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "ResourceId")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResource where
  hashWithSalt _salt DescribeResource' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DescribeResource where
  rnf DescribeResource' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders DescribeResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DescribeResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeResource where
  toJSON DescribeResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath DescribeResource where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourceResponse' smart constructor.
data DescribeResourceResponse = DescribeResourceResponse'
  { -- | The booking options for the described resource.
    bookingOptions :: Prelude.Maybe BookingOptions,
    -- | The date and time when a resource was disabled from WorkMail, in UNIX
    -- epoch time format.
    disabledDate :: Prelude.Maybe Data.POSIX,
    -- | The email of the described resource.
    email :: Prelude.Maybe Prelude.Text,
    -- | The date and time when a resource was enabled for WorkMail, in UNIX
    -- epoch time format.
    enabledDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the described resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the described resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The state of the resource: enabled (registered to WorkMail), disabled
    -- (deregistered or never registered to WorkMail), or deleted.
    state :: Prelude.Maybe EntityState,
    -- | The type of the described resource.
    type' :: Prelude.Maybe ResourceType,
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
-- 'bookingOptions', 'describeResourceResponse_bookingOptions' - The booking options for the described resource.
--
-- 'disabledDate', 'describeResourceResponse_disabledDate' - The date and time when a resource was disabled from WorkMail, in UNIX
-- epoch time format.
--
-- 'email', 'describeResourceResponse_email' - The email of the described resource.
--
-- 'enabledDate', 'describeResourceResponse_enabledDate' - The date and time when a resource was enabled for WorkMail, in UNIX
-- epoch time format.
--
-- 'name', 'describeResourceResponse_name' - The name of the described resource.
--
-- 'resourceId', 'describeResourceResponse_resourceId' - The identifier of the described resource.
--
-- 'state', 'describeResourceResponse_state' - The state of the resource: enabled (registered to WorkMail), disabled
-- (deregistered or never registered to WorkMail), or deleted.
--
-- 'type'', 'describeResourceResponse_type' - The type of the described resource.
--
-- 'httpStatus', 'describeResourceResponse_httpStatus' - The response's http status code.
newDescribeResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourceResponse
newDescribeResourceResponse pHttpStatus_ =
  DescribeResourceResponse'
    { bookingOptions =
        Prelude.Nothing,
      disabledDate = Prelude.Nothing,
      email = Prelude.Nothing,
      enabledDate = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The booking options for the described resource.
describeResourceResponse_bookingOptions :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe BookingOptions)
describeResourceResponse_bookingOptions = Lens.lens (\DescribeResourceResponse' {bookingOptions} -> bookingOptions) (\s@DescribeResourceResponse' {} a -> s {bookingOptions = a} :: DescribeResourceResponse)

-- | The date and time when a resource was disabled from WorkMail, in UNIX
-- epoch time format.
describeResourceResponse_disabledDate :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.UTCTime)
describeResourceResponse_disabledDate = Lens.lens (\DescribeResourceResponse' {disabledDate} -> disabledDate) (\s@DescribeResourceResponse' {} a -> s {disabledDate = a} :: DescribeResourceResponse) Prelude.. Lens.mapping Data._Time

-- | The email of the described resource.
describeResourceResponse_email :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.Text)
describeResourceResponse_email = Lens.lens (\DescribeResourceResponse' {email} -> email) (\s@DescribeResourceResponse' {} a -> s {email = a} :: DescribeResourceResponse)

-- | The date and time when a resource was enabled for WorkMail, in UNIX
-- epoch time format.
describeResourceResponse_enabledDate :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.UTCTime)
describeResourceResponse_enabledDate = Lens.lens (\DescribeResourceResponse' {enabledDate} -> enabledDate) (\s@DescribeResourceResponse' {} a -> s {enabledDate = a} :: DescribeResourceResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the described resource.
describeResourceResponse_name :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.Text)
describeResourceResponse_name = Lens.lens (\DescribeResourceResponse' {name} -> name) (\s@DescribeResourceResponse' {} a -> s {name = a} :: DescribeResourceResponse)

-- | The identifier of the described resource.
describeResourceResponse_resourceId :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe Prelude.Text)
describeResourceResponse_resourceId = Lens.lens (\DescribeResourceResponse' {resourceId} -> resourceId) (\s@DescribeResourceResponse' {} a -> s {resourceId = a} :: DescribeResourceResponse)

-- | The state of the resource: enabled (registered to WorkMail), disabled
-- (deregistered or never registered to WorkMail), or deleted.
describeResourceResponse_state :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe EntityState)
describeResourceResponse_state = Lens.lens (\DescribeResourceResponse' {state} -> state) (\s@DescribeResourceResponse' {} a -> s {state = a} :: DescribeResourceResponse)

-- | The type of the described resource.
describeResourceResponse_type :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe ResourceType)
describeResourceResponse_type = Lens.lens (\DescribeResourceResponse' {type'} -> type') (\s@DescribeResourceResponse' {} a -> s {type' = a} :: DescribeResourceResponse)

-- | The response's http status code.
describeResourceResponse_httpStatus :: Lens.Lens' DescribeResourceResponse Prelude.Int
describeResourceResponse_httpStatus = Lens.lens (\DescribeResourceResponse' {httpStatus} -> httpStatus) (\s@DescribeResourceResponse' {} a -> s {httpStatus = a} :: DescribeResourceResponse)

instance Prelude.NFData DescribeResourceResponse where
  rnf DescribeResourceResponse' {..} =
    Prelude.rnf bookingOptions
      `Prelude.seq` Prelude.rnf disabledDate
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf enabledDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
