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
-- Module      : Network.AWS.OpsWorksCM.DescribeAccountAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your OpsWorks-CM account attributes.
--
-- This operation is synchronous.
module Network.AWS.OpsWorksCM.DescribeAccountAttributes
  ( -- * Creating a Request
    DescribeAccountAttributes (..),
    newDescribeAccountAttributes,

    -- * Destructuring the Response
    DescribeAccountAttributesResponse (..),
    newDescribeAccountAttributesResponse,

    -- * Response Lenses
    describeAccountAttributesResponse_attributes,
    describeAccountAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAccountAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAccountAttributes ::
  DescribeAccountAttributes
newDescribeAccountAttributes =
  DescribeAccountAttributes'

instance Core.AWSRequest DescribeAccountAttributes where
  type
    AWSResponse DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Core.<$> (x Core..?> "Attributes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAccountAttributes

instance Core.NFData DescribeAccountAttributes

instance Core.ToHeaders DescribeAccountAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.DescribeAccountAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAccountAttributes where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeAccountAttributes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAccountAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | The attributes that are currently set for the account.
    attributes :: Core.Maybe [AccountAttribute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAccountAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'describeAccountAttributesResponse_attributes' - The attributes that are currently set for the account.
--
-- 'httpStatus', 'describeAccountAttributesResponse_httpStatus' - The response's http status code.
newDescribeAccountAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAccountAttributesResponse
newDescribeAccountAttributesResponse pHttpStatus_ =
  DescribeAccountAttributesResponse'
    { attributes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attributes that are currently set for the account.
describeAccountAttributesResponse_attributes :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe [AccountAttribute])
describeAccountAttributesResponse_attributes = Lens.lens (\DescribeAccountAttributesResponse' {attributes} -> attributes) (\s@DescribeAccountAttributesResponse' {} a -> s {attributes = a} :: DescribeAccountAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAccountAttributesResponse_httpStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
describeAccountAttributesResponse_httpStatus = Lens.lens (\DescribeAccountAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAttributesResponse' {} a -> s {httpStatus = a} :: DescribeAccountAttributesResponse)

instance
  Core.NFData
    DescribeAccountAttributesResponse
