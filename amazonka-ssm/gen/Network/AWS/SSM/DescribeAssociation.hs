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
-- Module      : Network.AWS.SSM.DescribeAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the association for the specified target or instance. If you
-- created the association by using the @Targets@ parameter, then you must
-- retrieve the association by using the association ID. If you created the
-- association by specifying an instance ID and a Systems Manager document,
-- then you retrieve the association by specifying the document name and
-- the instance ID.
module Network.AWS.SSM.DescribeAssociation
  ( -- * Creating a Request
    DescribeAssociation (..),
    newDescribeAssociation,

    -- * Request Lenses
    describeAssociation_instanceId,
    describeAssociation_name,
    describeAssociation_associationId,
    describeAssociation_associationVersion,

    -- * Destructuring the Response
    DescribeAssociationResponse (..),
    newDescribeAssociationResponse,

    -- * Response Lenses
    describeAssociationResponse_associationDescription,
    describeAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeAssociation' smart constructor.
data DescribeAssociation = DescribeAssociation'
  { -- | The instance ID.
    instanceId :: Core.Maybe Core.Text,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Core.Text,
    -- | The association ID for which you want information.
    associationId :: Core.Maybe Core.Text,
    -- | Specify the association version to retrieve. To view the latest version,
    -- either specify @$LATEST@ for this parameter, or omit this parameter. To
    -- view a list of all associations for an instance, use ListAssociations.
    -- To get a list of versions for a specific association, use
    -- ListAssociationVersions.
    associationVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeAssociation_instanceId' - The instance ID.
--
-- 'name', 'describeAssociation_name' - The name of the Systems Manager document.
--
-- 'associationId', 'describeAssociation_associationId' - The association ID for which you want information.
--
-- 'associationVersion', 'describeAssociation_associationVersion' - Specify the association version to retrieve. To view the latest version,
-- either specify @$LATEST@ for this parameter, or omit this parameter. To
-- view a list of all associations for an instance, use ListAssociations.
-- To get a list of versions for a specific association, use
-- ListAssociationVersions.
newDescribeAssociation ::
  DescribeAssociation
newDescribeAssociation =
  DescribeAssociation'
    { instanceId = Core.Nothing,
      name = Core.Nothing,
      associationId = Core.Nothing,
      associationVersion = Core.Nothing
    }

-- | The instance ID.
describeAssociation_instanceId :: Lens.Lens' DescribeAssociation (Core.Maybe Core.Text)
describeAssociation_instanceId = Lens.lens (\DescribeAssociation' {instanceId} -> instanceId) (\s@DescribeAssociation' {} a -> s {instanceId = a} :: DescribeAssociation)

-- | The name of the Systems Manager document.
describeAssociation_name :: Lens.Lens' DescribeAssociation (Core.Maybe Core.Text)
describeAssociation_name = Lens.lens (\DescribeAssociation' {name} -> name) (\s@DescribeAssociation' {} a -> s {name = a} :: DescribeAssociation)

-- | The association ID for which you want information.
describeAssociation_associationId :: Lens.Lens' DescribeAssociation (Core.Maybe Core.Text)
describeAssociation_associationId = Lens.lens (\DescribeAssociation' {associationId} -> associationId) (\s@DescribeAssociation' {} a -> s {associationId = a} :: DescribeAssociation)

-- | Specify the association version to retrieve. To view the latest version,
-- either specify @$LATEST@ for this parameter, or omit this parameter. To
-- view a list of all associations for an instance, use ListAssociations.
-- To get a list of versions for a specific association, use
-- ListAssociationVersions.
describeAssociation_associationVersion :: Lens.Lens' DescribeAssociation (Core.Maybe Core.Text)
describeAssociation_associationVersion = Lens.lens (\DescribeAssociation' {associationVersion} -> associationVersion) (\s@DescribeAssociation' {} a -> s {associationVersion = a} :: DescribeAssociation)

instance Core.AWSRequest DescribeAssociation where
  type
    AWSResponse DescribeAssociation =
      DescribeAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssociationResponse'
            Core.<$> (x Core..?> "AssociationDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAssociation

instance Core.NFData DescribeAssociation

instance Core.ToHeaders DescribeAssociation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DescribeAssociation" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAssociation where
  toJSON DescribeAssociation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceId" Core..=) Core.<$> instanceId,
            ("Name" Core..=) Core.<$> name,
            ("AssociationId" Core..=) Core.<$> associationId,
            ("AssociationVersion" Core..=)
              Core.<$> associationVersion
          ]
      )

instance Core.ToPath DescribeAssociation where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAssociation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAssociationResponse' smart constructor.
data DescribeAssociationResponse = DescribeAssociationResponse'
  { -- | Information about the association.
    associationDescription :: Core.Maybe AssociationDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationDescription', 'describeAssociationResponse_associationDescription' - Information about the association.
--
-- 'httpStatus', 'describeAssociationResponse_httpStatus' - The response's http status code.
newDescribeAssociationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAssociationResponse
newDescribeAssociationResponse pHttpStatus_ =
  DescribeAssociationResponse'
    { associationDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the association.
describeAssociationResponse_associationDescription :: Lens.Lens' DescribeAssociationResponse (Core.Maybe AssociationDescription)
describeAssociationResponse_associationDescription = Lens.lens (\DescribeAssociationResponse' {associationDescription} -> associationDescription) (\s@DescribeAssociationResponse' {} a -> s {associationDescription = a} :: DescribeAssociationResponse)

-- | The response's http status code.
describeAssociationResponse_httpStatus :: Lens.Lens' DescribeAssociationResponse Core.Int
describeAssociationResponse_httpStatus = Lens.lens (\DescribeAssociationResponse' {httpStatus} -> httpStatus) (\s@DescribeAssociationResponse' {} a -> s {httpStatus = a} :: DescribeAssociationResponse)

instance Core.NFData DescribeAssociationResponse
