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
-- Module      : Amazonka.IoTSiteWise.DescribeAccessPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an access policy, which specifies an identity\'s access to an
-- IoT SiteWise Monitor portal or project.
module Amazonka.IoTSiteWise.DescribeAccessPolicy
  ( -- * Creating a Request
    DescribeAccessPolicy (..),
    newDescribeAccessPolicy,

    -- * Request Lenses
    describeAccessPolicy_accessPolicyId,

    -- * Destructuring the Response
    DescribeAccessPolicyResponse (..),
    newDescribeAccessPolicyResponse,

    -- * Response Lenses
    describeAccessPolicyResponse_httpStatus,
    describeAccessPolicyResponse_accessPolicyId,
    describeAccessPolicyResponse_accessPolicyArn,
    describeAccessPolicyResponse_accessPolicyIdentity,
    describeAccessPolicyResponse_accessPolicyResource,
    describeAccessPolicyResponse_accessPolicyPermission,
    describeAccessPolicyResponse_accessPolicyCreationDate,
    describeAccessPolicyResponse_accessPolicyLastUpdateDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccessPolicy' smart constructor.
data DescribeAccessPolicy = DescribeAccessPolicy'
  { -- | The ID of the access policy.
    accessPolicyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicyId', 'describeAccessPolicy_accessPolicyId' - The ID of the access policy.
newDescribeAccessPolicy ::
  -- | 'accessPolicyId'
  Prelude.Text ->
  DescribeAccessPolicy
newDescribeAccessPolicy pAccessPolicyId_ =
  DescribeAccessPolicy'
    { accessPolicyId =
        pAccessPolicyId_
    }

-- | The ID of the access policy.
describeAccessPolicy_accessPolicyId :: Lens.Lens' DescribeAccessPolicy Prelude.Text
describeAccessPolicy_accessPolicyId = Lens.lens (\DescribeAccessPolicy' {accessPolicyId} -> accessPolicyId) (\s@DescribeAccessPolicy' {} a -> s {accessPolicyId = a} :: DescribeAccessPolicy)

instance Core.AWSRequest DescribeAccessPolicy where
  type
    AWSResponse DescribeAccessPolicy =
      DescribeAccessPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccessPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "accessPolicyId")
            Prelude.<*> (x Core..:> "accessPolicyArn")
            Prelude.<*> (x Core..:> "accessPolicyIdentity")
            Prelude.<*> (x Core..:> "accessPolicyResource")
            Prelude.<*> (x Core..:> "accessPolicyPermission")
            Prelude.<*> (x Core..:> "accessPolicyCreationDate")
            Prelude.<*> (x Core..:> "accessPolicyLastUpdateDate")
      )

instance Prelude.Hashable DescribeAccessPolicy where
  hashWithSalt _salt DescribeAccessPolicy' {..} =
    _salt `Prelude.hashWithSalt` accessPolicyId

instance Prelude.NFData DescribeAccessPolicy where
  rnf DescribeAccessPolicy' {..} =
    Prelude.rnf accessPolicyId

instance Core.ToHeaders DescribeAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeAccessPolicy where
  toPath DescribeAccessPolicy' {..} =
    Prelude.mconcat
      ["/access-policies/", Core.toBS accessPolicyId]

instance Core.ToQuery DescribeAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccessPolicyResponse' smart constructor.
data DescribeAccessPolicyResponse = DescribeAccessPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the access policy.
    accessPolicyId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the access policy, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:access-policy\/${AccessPolicyId}@
    accessPolicyArn :: Prelude.Text,
    -- | The identity (IAM Identity Center user, IAM Identity Center group, or
    -- IAM user) to which this access policy applies.
    accessPolicyIdentity :: Identity,
    -- | The IoT SiteWise Monitor resource (portal or project) to which this
    -- access policy provides access.
    accessPolicyResource :: Resource,
    -- | The access policy permission. Note that a project @ADMINISTRATOR@ is
    -- also known as a project owner.
    accessPolicyPermission :: Permission,
    -- | The date the access policy was created, in Unix epoch time.
    accessPolicyCreationDate :: Core.POSIX,
    -- | The date the access policy was last updated, in Unix epoch time.
    accessPolicyLastUpdateDate :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAccessPolicyResponse_httpStatus' - The response's http status code.
--
-- 'accessPolicyId', 'describeAccessPolicyResponse_accessPolicyId' - The ID of the access policy.
--
-- 'accessPolicyArn', 'describeAccessPolicyResponse_accessPolicyArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the access policy, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:access-policy\/${AccessPolicyId}@
--
-- 'accessPolicyIdentity', 'describeAccessPolicyResponse_accessPolicyIdentity' - The identity (IAM Identity Center user, IAM Identity Center group, or
-- IAM user) to which this access policy applies.
--
-- 'accessPolicyResource', 'describeAccessPolicyResponse_accessPolicyResource' - The IoT SiteWise Monitor resource (portal or project) to which this
-- access policy provides access.
--
-- 'accessPolicyPermission', 'describeAccessPolicyResponse_accessPolicyPermission' - The access policy permission. Note that a project @ADMINISTRATOR@ is
-- also known as a project owner.
--
-- 'accessPolicyCreationDate', 'describeAccessPolicyResponse_accessPolicyCreationDate' - The date the access policy was created, in Unix epoch time.
--
-- 'accessPolicyLastUpdateDate', 'describeAccessPolicyResponse_accessPolicyLastUpdateDate' - The date the access policy was last updated, in Unix epoch time.
newDescribeAccessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accessPolicyId'
  Prelude.Text ->
  -- | 'accessPolicyArn'
  Prelude.Text ->
  -- | 'accessPolicyIdentity'
  Identity ->
  -- | 'accessPolicyResource'
  Resource ->
  -- | 'accessPolicyPermission'
  Permission ->
  -- | 'accessPolicyCreationDate'
  Prelude.UTCTime ->
  -- | 'accessPolicyLastUpdateDate'
  Prelude.UTCTime ->
  DescribeAccessPolicyResponse
newDescribeAccessPolicyResponse
  pHttpStatus_
  pAccessPolicyId_
  pAccessPolicyArn_
  pAccessPolicyIdentity_
  pAccessPolicyResource_
  pAccessPolicyPermission_
  pAccessPolicyCreationDate_
  pAccessPolicyLastUpdateDate_ =
    DescribeAccessPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        accessPolicyId = pAccessPolicyId_,
        accessPolicyArn = pAccessPolicyArn_,
        accessPolicyIdentity = pAccessPolicyIdentity_,
        accessPolicyResource = pAccessPolicyResource_,
        accessPolicyPermission =
          pAccessPolicyPermission_,
        accessPolicyCreationDate =
          Core._Time
            Lens.# pAccessPolicyCreationDate_,
        accessPolicyLastUpdateDate =
          Core._Time
            Lens.# pAccessPolicyLastUpdateDate_
      }

-- | The response's http status code.
describeAccessPolicyResponse_httpStatus :: Lens.Lens' DescribeAccessPolicyResponse Prelude.Int
describeAccessPolicyResponse_httpStatus = Lens.lens (\DescribeAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@DescribeAccessPolicyResponse' {} a -> s {httpStatus = a} :: DescribeAccessPolicyResponse)

-- | The ID of the access policy.
describeAccessPolicyResponse_accessPolicyId :: Lens.Lens' DescribeAccessPolicyResponse Prelude.Text
describeAccessPolicyResponse_accessPolicyId = Lens.lens (\DescribeAccessPolicyResponse' {accessPolicyId} -> accessPolicyId) (\s@DescribeAccessPolicyResponse' {} a -> s {accessPolicyId = a} :: DescribeAccessPolicyResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the access policy, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:access-policy\/${AccessPolicyId}@
describeAccessPolicyResponse_accessPolicyArn :: Lens.Lens' DescribeAccessPolicyResponse Prelude.Text
describeAccessPolicyResponse_accessPolicyArn = Lens.lens (\DescribeAccessPolicyResponse' {accessPolicyArn} -> accessPolicyArn) (\s@DescribeAccessPolicyResponse' {} a -> s {accessPolicyArn = a} :: DescribeAccessPolicyResponse)

-- | The identity (IAM Identity Center user, IAM Identity Center group, or
-- IAM user) to which this access policy applies.
describeAccessPolicyResponse_accessPolicyIdentity :: Lens.Lens' DescribeAccessPolicyResponse Identity
describeAccessPolicyResponse_accessPolicyIdentity = Lens.lens (\DescribeAccessPolicyResponse' {accessPolicyIdentity} -> accessPolicyIdentity) (\s@DescribeAccessPolicyResponse' {} a -> s {accessPolicyIdentity = a} :: DescribeAccessPolicyResponse)

-- | The IoT SiteWise Monitor resource (portal or project) to which this
-- access policy provides access.
describeAccessPolicyResponse_accessPolicyResource :: Lens.Lens' DescribeAccessPolicyResponse Resource
describeAccessPolicyResponse_accessPolicyResource = Lens.lens (\DescribeAccessPolicyResponse' {accessPolicyResource} -> accessPolicyResource) (\s@DescribeAccessPolicyResponse' {} a -> s {accessPolicyResource = a} :: DescribeAccessPolicyResponse)

-- | The access policy permission. Note that a project @ADMINISTRATOR@ is
-- also known as a project owner.
describeAccessPolicyResponse_accessPolicyPermission :: Lens.Lens' DescribeAccessPolicyResponse Permission
describeAccessPolicyResponse_accessPolicyPermission = Lens.lens (\DescribeAccessPolicyResponse' {accessPolicyPermission} -> accessPolicyPermission) (\s@DescribeAccessPolicyResponse' {} a -> s {accessPolicyPermission = a} :: DescribeAccessPolicyResponse)

-- | The date the access policy was created, in Unix epoch time.
describeAccessPolicyResponse_accessPolicyCreationDate :: Lens.Lens' DescribeAccessPolicyResponse Prelude.UTCTime
describeAccessPolicyResponse_accessPolicyCreationDate = Lens.lens (\DescribeAccessPolicyResponse' {accessPolicyCreationDate} -> accessPolicyCreationDate) (\s@DescribeAccessPolicyResponse' {} a -> s {accessPolicyCreationDate = a} :: DescribeAccessPolicyResponse) Prelude.. Core._Time

-- | The date the access policy was last updated, in Unix epoch time.
describeAccessPolicyResponse_accessPolicyLastUpdateDate :: Lens.Lens' DescribeAccessPolicyResponse Prelude.UTCTime
describeAccessPolicyResponse_accessPolicyLastUpdateDate = Lens.lens (\DescribeAccessPolicyResponse' {accessPolicyLastUpdateDate} -> accessPolicyLastUpdateDate) (\s@DescribeAccessPolicyResponse' {} a -> s {accessPolicyLastUpdateDate = a} :: DescribeAccessPolicyResponse) Prelude.. Core._Time

instance Prelude.NFData DescribeAccessPolicyResponse where
  rnf DescribeAccessPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accessPolicyId
      `Prelude.seq` Prelude.rnf accessPolicyArn
      `Prelude.seq` Prelude.rnf accessPolicyIdentity
      `Prelude.seq` Prelude.rnf accessPolicyResource
      `Prelude.seq` Prelude.rnf accessPolicyPermission
      `Prelude.seq` Prelude.rnf accessPolicyCreationDate
      `Prelude.seq` Prelude.rnf accessPolicyLastUpdateDate
