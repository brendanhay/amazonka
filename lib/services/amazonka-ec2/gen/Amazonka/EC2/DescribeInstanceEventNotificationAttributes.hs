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
-- Module      : Amazonka.EC2.DescribeInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tag keys that are registered to appear in scheduled event
-- notifications for resources in the current Region.
module Amazonka.EC2.DescribeInstanceEventNotificationAttributes
  ( -- * Creating a Request
    DescribeInstanceEventNotificationAttributes (..),
    newDescribeInstanceEventNotificationAttributes,

    -- * Request Lenses
    describeInstanceEventNotificationAttributes_dryRun,

    -- * Destructuring the Response
    DescribeInstanceEventNotificationAttributesResponse (..),
    newDescribeInstanceEventNotificationAttributesResponse,

    -- * Response Lenses
    describeInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    describeInstanceEventNotificationAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceEventNotificationAttributes' smart constructor.
data DescribeInstanceEventNotificationAttributes = DescribeInstanceEventNotificationAttributes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceEventNotificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeInstanceEventNotificationAttributes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDescribeInstanceEventNotificationAttributes ::
  DescribeInstanceEventNotificationAttributes
newDescribeInstanceEventNotificationAttributes =
  DescribeInstanceEventNotificationAttributes'
    { dryRun =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceEventNotificationAttributes_dryRun :: Lens.Lens' DescribeInstanceEventNotificationAttributes (Prelude.Maybe Prelude.Bool)
describeInstanceEventNotificationAttributes_dryRun = Lens.lens (\DescribeInstanceEventNotificationAttributes' {dryRun} -> dryRun) (\s@DescribeInstanceEventNotificationAttributes' {} a -> s {dryRun = a} :: DescribeInstanceEventNotificationAttributes)

instance
  Core.AWSRequest
    DescribeInstanceEventNotificationAttributes
  where
  type
    AWSResponse
      DescribeInstanceEventNotificationAttributes =
      DescribeInstanceEventNotificationAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceEventNotificationAttributesResponse'
            Prelude.<$> (x Data..@? "instanceTagAttribute")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeInstanceEventNotificationAttributes
  where
  hashWithSalt
    _salt
    DescribeInstanceEventNotificationAttributes' {..} =
      _salt `Prelude.hashWithSalt` dryRun

instance
  Prelude.NFData
    DescribeInstanceEventNotificationAttributes
  where
  rnf DescribeInstanceEventNotificationAttributes' {..} =
    Prelude.rnf dryRun

instance
  Data.ToHeaders
    DescribeInstanceEventNotificationAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeInstanceEventNotificationAttributes
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeInstanceEventNotificationAttributes
  where
  toQuery
    DescribeInstanceEventNotificationAttributes' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeInstanceEventNotificationAttributes" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun
        ]

-- | /See:/ 'newDescribeInstanceEventNotificationAttributesResponse' smart constructor.
data DescribeInstanceEventNotificationAttributesResponse = DescribeInstanceEventNotificationAttributesResponse'
  { -- | Information about the registered tag keys.
    instanceTagAttribute :: Prelude.Maybe InstanceTagNotificationAttribute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceEventNotificationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTagAttribute', 'describeInstanceEventNotificationAttributesResponse_instanceTagAttribute' - Information about the registered tag keys.
--
-- 'httpStatus', 'describeInstanceEventNotificationAttributesResponse_httpStatus' - The response's http status code.
newDescribeInstanceEventNotificationAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceEventNotificationAttributesResponse
newDescribeInstanceEventNotificationAttributesResponse
  pHttpStatus_ =
    DescribeInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the registered tag keys.
describeInstanceEventNotificationAttributesResponse_instanceTagAttribute :: Lens.Lens' DescribeInstanceEventNotificationAttributesResponse (Prelude.Maybe InstanceTagNotificationAttribute)
describeInstanceEventNotificationAttributesResponse_instanceTagAttribute = Lens.lens (\DescribeInstanceEventNotificationAttributesResponse' {instanceTagAttribute} -> instanceTagAttribute) (\s@DescribeInstanceEventNotificationAttributesResponse' {} a -> s {instanceTagAttribute = a} :: DescribeInstanceEventNotificationAttributesResponse)

-- | The response's http status code.
describeInstanceEventNotificationAttributesResponse_httpStatus :: Lens.Lens' DescribeInstanceEventNotificationAttributesResponse Prelude.Int
describeInstanceEventNotificationAttributesResponse_httpStatus = Lens.lens (\DescribeInstanceEventNotificationAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceEventNotificationAttributesResponse' {} a -> s {httpStatus = a} :: DescribeInstanceEventNotificationAttributesResponse)

instance
  Prelude.NFData
    DescribeInstanceEventNotificationAttributesResponse
  where
  rnf
    DescribeInstanceEventNotificationAttributesResponse' {..} =
      Prelude.rnf instanceTagAttribute
        `Prelude.seq` Prelude.rnf httpStatus
