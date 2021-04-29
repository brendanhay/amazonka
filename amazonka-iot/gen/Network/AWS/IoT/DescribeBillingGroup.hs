{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.DescribeBillingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a billing group.
module Network.AWS.IoT.DescribeBillingGroup
  ( -- * Creating a Request
    DescribeBillingGroup (..),
    newDescribeBillingGroup,

    -- * Request Lenses
    describeBillingGroup_billingGroupName,

    -- * Destructuring the Response
    DescribeBillingGroupResponse (..),
    newDescribeBillingGroupResponse,

    -- * Response Lenses
    describeBillingGroupResponse_billingGroupProperties,
    describeBillingGroupResponse_version,
    describeBillingGroupResponse_billingGroupArn,
    describeBillingGroupResponse_billingGroupId,
    describeBillingGroupResponse_billingGroupMetadata,
    describeBillingGroupResponse_billingGroupName,
    describeBillingGroupResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBillingGroup' smart constructor.
data DescribeBillingGroup = DescribeBillingGroup'
  { -- | The name of the billing group.
    billingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupName', 'describeBillingGroup_billingGroupName' - The name of the billing group.
newDescribeBillingGroup ::
  -- | 'billingGroupName'
  Prelude.Text ->
  DescribeBillingGroup
newDescribeBillingGroup pBillingGroupName_ =
  DescribeBillingGroup'
    { billingGroupName =
        pBillingGroupName_
    }

-- | The name of the billing group.
describeBillingGroup_billingGroupName :: Lens.Lens' DescribeBillingGroup Prelude.Text
describeBillingGroup_billingGroupName = Lens.lens (\DescribeBillingGroup' {billingGroupName} -> billingGroupName) (\s@DescribeBillingGroup' {} a -> s {billingGroupName = a} :: DescribeBillingGroup)

instance Prelude.AWSRequest DescribeBillingGroup where
  type
    Rs DescribeBillingGroup =
      DescribeBillingGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBillingGroupResponse'
            Prelude.<$> (x Prelude..?> "billingGroupProperties")
            Prelude.<*> (x Prelude..?> "version")
            Prelude.<*> (x Prelude..?> "billingGroupArn")
            Prelude.<*> (x Prelude..?> "billingGroupId")
            Prelude.<*> (x Prelude..?> "billingGroupMetadata")
            Prelude.<*> (x Prelude..?> "billingGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBillingGroup

instance Prelude.NFData DescribeBillingGroup

instance Prelude.ToHeaders DescribeBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeBillingGroup where
  toPath DescribeBillingGroup' {..} =
    Prelude.mconcat
      ["/billing-groups/", Prelude.toBS billingGroupName]

instance Prelude.ToQuery DescribeBillingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBillingGroupResponse' smart constructor.
data DescribeBillingGroupResponse = DescribeBillingGroupResponse'
  { -- | The properties of the billing group.
    billingGroupProperties :: Prelude.Maybe BillingGroupProperties,
    -- | The version of the billing group.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The ARN of the billing group.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the billing group.
    billingGroupId :: Prelude.Maybe Prelude.Text,
    -- | Additional information about the billing group.
    billingGroupMetadata :: Prelude.Maybe BillingGroupMetadata,
    -- | The name of the billing group.
    billingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupProperties', 'describeBillingGroupResponse_billingGroupProperties' - The properties of the billing group.
--
-- 'version', 'describeBillingGroupResponse_version' - The version of the billing group.
--
-- 'billingGroupArn', 'describeBillingGroupResponse_billingGroupArn' - The ARN of the billing group.
--
-- 'billingGroupId', 'describeBillingGroupResponse_billingGroupId' - The ID of the billing group.
--
-- 'billingGroupMetadata', 'describeBillingGroupResponse_billingGroupMetadata' - Additional information about the billing group.
--
-- 'billingGroupName', 'describeBillingGroupResponse_billingGroupName' - The name of the billing group.
--
-- 'httpStatus', 'describeBillingGroupResponse_httpStatus' - The response's http status code.
newDescribeBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBillingGroupResponse
newDescribeBillingGroupResponse pHttpStatus_ =
  DescribeBillingGroupResponse'
    { billingGroupProperties =
        Prelude.Nothing,
      version = Prelude.Nothing,
      billingGroupArn = Prelude.Nothing,
      billingGroupId = Prelude.Nothing,
      billingGroupMetadata = Prelude.Nothing,
      billingGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of the billing group.
describeBillingGroupResponse_billingGroupProperties :: Lens.Lens' DescribeBillingGroupResponse (Prelude.Maybe BillingGroupProperties)
describeBillingGroupResponse_billingGroupProperties = Lens.lens (\DescribeBillingGroupResponse' {billingGroupProperties} -> billingGroupProperties) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupProperties = a} :: DescribeBillingGroupResponse)

-- | The version of the billing group.
describeBillingGroupResponse_version :: Lens.Lens' DescribeBillingGroupResponse (Prelude.Maybe Prelude.Integer)
describeBillingGroupResponse_version = Lens.lens (\DescribeBillingGroupResponse' {version} -> version) (\s@DescribeBillingGroupResponse' {} a -> s {version = a} :: DescribeBillingGroupResponse)

-- | The ARN of the billing group.
describeBillingGroupResponse_billingGroupArn :: Lens.Lens' DescribeBillingGroupResponse (Prelude.Maybe Prelude.Text)
describeBillingGroupResponse_billingGroupArn = Lens.lens (\DescribeBillingGroupResponse' {billingGroupArn} -> billingGroupArn) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupArn = a} :: DescribeBillingGroupResponse)

-- | The ID of the billing group.
describeBillingGroupResponse_billingGroupId :: Lens.Lens' DescribeBillingGroupResponse (Prelude.Maybe Prelude.Text)
describeBillingGroupResponse_billingGroupId = Lens.lens (\DescribeBillingGroupResponse' {billingGroupId} -> billingGroupId) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupId = a} :: DescribeBillingGroupResponse)

-- | Additional information about the billing group.
describeBillingGroupResponse_billingGroupMetadata :: Lens.Lens' DescribeBillingGroupResponse (Prelude.Maybe BillingGroupMetadata)
describeBillingGroupResponse_billingGroupMetadata = Lens.lens (\DescribeBillingGroupResponse' {billingGroupMetadata} -> billingGroupMetadata) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupMetadata = a} :: DescribeBillingGroupResponse)

-- | The name of the billing group.
describeBillingGroupResponse_billingGroupName :: Lens.Lens' DescribeBillingGroupResponse (Prelude.Maybe Prelude.Text)
describeBillingGroupResponse_billingGroupName = Lens.lens (\DescribeBillingGroupResponse' {billingGroupName} -> billingGroupName) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupName = a} :: DescribeBillingGroupResponse)

-- | The response's http status code.
describeBillingGroupResponse_httpStatus :: Lens.Lens' DescribeBillingGroupResponse Prelude.Int
describeBillingGroupResponse_httpStatus = Lens.lens (\DescribeBillingGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeBillingGroupResponse' {} a -> s {httpStatus = a} :: DescribeBillingGroupResponse)

instance Prelude.NFData DescribeBillingGroupResponse
