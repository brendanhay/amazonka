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
-- Module      : Amazonka.AutoScaling.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification types that are supported by Amazon EC2 Auto
-- Scaling.
module Amazonka.AutoScaling.DescribeAutoScalingNotificationTypes
  ( -- * Creating a Request
    DescribeAutoScalingNotificationTypes (..),
    newDescribeAutoScalingNotificationTypes,

    -- * Destructuring the Response
    DescribeAutoScalingNotificationTypesResponse (..),
    newDescribeAutoScalingNotificationTypesResponse,

    -- * Response Lenses
    describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes,
    describeAutoScalingNotificationTypesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAutoScalingNotificationTypes' smart constructor.
data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoScalingNotificationTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAutoScalingNotificationTypes ::
  DescribeAutoScalingNotificationTypes
newDescribeAutoScalingNotificationTypes =
  DescribeAutoScalingNotificationTypes'

instance
  Core.AWSRequest
    DescribeAutoScalingNotificationTypes
  where
  type
    AWSResponse DescribeAutoScalingNotificationTypes =
      DescribeAutoScalingNotificationTypesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAutoScalingNotificationTypesResult"
      ( \s h x ->
          DescribeAutoScalingNotificationTypesResponse'
            Prelude.<$> ( x Data..@? "AutoScalingNotificationTypes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAutoScalingNotificationTypes
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DescribeAutoScalingNotificationTypes
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DescribeAutoScalingNotificationTypes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeAutoScalingNotificationTypes
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAutoScalingNotificationTypes
  where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ( "DescribeAutoScalingNotificationTypes" ::
                          Prelude.ByteString
                      ),
            "Version"
              Data.=: ("2011-01-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeAutoScalingNotificationTypesResponse' smart constructor.
data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse'
  { -- | The notification types.
    autoScalingNotificationTypes :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoScalingNotificationTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingNotificationTypes', 'describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes' - The notification types.
--
-- 'httpStatus', 'describeAutoScalingNotificationTypesResponse_httpStatus' - The response's http status code.
newDescribeAutoScalingNotificationTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAutoScalingNotificationTypesResponse
newDescribeAutoScalingNotificationTypesResponse
  pHttpStatus_ =
    DescribeAutoScalingNotificationTypesResponse'
      { autoScalingNotificationTypes =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The notification types.
describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes :: Lens.Lens' DescribeAutoScalingNotificationTypesResponse (Prelude.Maybe [Prelude.Text])
describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes = Lens.lens (\DescribeAutoScalingNotificationTypesResponse' {autoScalingNotificationTypes} -> autoScalingNotificationTypes) (\s@DescribeAutoScalingNotificationTypesResponse' {} a -> s {autoScalingNotificationTypes = a} :: DescribeAutoScalingNotificationTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAutoScalingNotificationTypesResponse_httpStatus :: Lens.Lens' DescribeAutoScalingNotificationTypesResponse Prelude.Int
describeAutoScalingNotificationTypesResponse_httpStatus = Lens.lens (\DescribeAutoScalingNotificationTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeAutoScalingNotificationTypesResponse' {} a -> s {httpStatus = a} :: DescribeAutoScalingNotificationTypesResponse)

instance
  Prelude.NFData
    DescribeAutoScalingNotificationTypesResponse
  where
  rnf DescribeAutoScalingNotificationTypesResponse' {..} =
    Prelude.rnf autoScalingNotificationTypes
      `Prelude.seq` Prelude.rnf httpStatus
