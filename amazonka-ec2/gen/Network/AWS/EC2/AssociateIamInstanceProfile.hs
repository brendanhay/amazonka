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
-- Module      : Network.AWS.EC2.AssociateIamInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an IAM instance profile with a running or stopped instance.
-- You cannot associate more than one IAM instance profile with an
-- instance.
module Network.AWS.EC2.AssociateIamInstanceProfile
  ( -- * Creating a Request
    AssociateIamInstanceProfile (..),
    newAssociateIamInstanceProfile,

    -- * Request Lenses
    associateIamInstanceProfile_iamInstanceProfile,
    associateIamInstanceProfile_instanceId,

    -- * Destructuring the Response
    AssociateIamInstanceProfileResponse (..),
    newAssociateIamInstanceProfileResponse,

    -- * Response Lenses
    associateIamInstanceProfileResponse_iamInstanceProfileAssociation,
    associateIamInstanceProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateIamInstanceProfile' smart constructor.
data AssociateIamInstanceProfile = AssociateIamInstanceProfile'
  { -- | The IAM instance profile.
    iamInstanceProfile :: IamInstanceProfileSpecification,
    -- | The ID of the instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateIamInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamInstanceProfile', 'associateIamInstanceProfile_iamInstanceProfile' - The IAM instance profile.
--
-- 'instanceId', 'associateIamInstanceProfile_instanceId' - The ID of the instance.
newAssociateIamInstanceProfile ::
  -- | 'iamInstanceProfile'
  IamInstanceProfileSpecification ->
  -- | 'instanceId'
  Core.Text ->
  AssociateIamInstanceProfile
newAssociateIamInstanceProfile
  pIamInstanceProfile_
  pInstanceId_ =
    AssociateIamInstanceProfile'
      { iamInstanceProfile =
          pIamInstanceProfile_,
        instanceId = pInstanceId_
      }

-- | The IAM instance profile.
associateIamInstanceProfile_iamInstanceProfile :: Lens.Lens' AssociateIamInstanceProfile IamInstanceProfileSpecification
associateIamInstanceProfile_iamInstanceProfile = Lens.lens (\AssociateIamInstanceProfile' {iamInstanceProfile} -> iamInstanceProfile) (\s@AssociateIamInstanceProfile' {} a -> s {iamInstanceProfile = a} :: AssociateIamInstanceProfile)

-- | The ID of the instance.
associateIamInstanceProfile_instanceId :: Lens.Lens' AssociateIamInstanceProfile Core.Text
associateIamInstanceProfile_instanceId = Lens.lens (\AssociateIamInstanceProfile' {instanceId} -> instanceId) (\s@AssociateIamInstanceProfile' {} a -> s {instanceId = a} :: AssociateIamInstanceProfile)

instance Core.AWSRequest AssociateIamInstanceProfile where
  type
    AWSResponse AssociateIamInstanceProfile =
      AssociateIamInstanceProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateIamInstanceProfileResponse'
            Core.<$> (x Core..@? "iamInstanceProfileAssociation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateIamInstanceProfile

instance Core.NFData AssociateIamInstanceProfile

instance Core.ToHeaders AssociateIamInstanceProfile where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AssociateIamInstanceProfile where
  toPath = Core.const "/"

instance Core.ToQuery AssociateIamInstanceProfile where
  toQuery AssociateIamInstanceProfile' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AssociateIamInstanceProfile" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "IamInstanceProfile" Core.=: iamInstanceProfile,
        "InstanceId" Core.=: instanceId
      ]

-- | /See:/ 'newAssociateIamInstanceProfileResponse' smart constructor.
data AssociateIamInstanceProfileResponse = AssociateIamInstanceProfileResponse'
  { -- | Information about the IAM instance profile association.
    iamInstanceProfileAssociation :: Core.Maybe IamInstanceProfileAssociation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateIamInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamInstanceProfileAssociation', 'associateIamInstanceProfileResponse_iamInstanceProfileAssociation' - Information about the IAM instance profile association.
--
-- 'httpStatus', 'associateIamInstanceProfileResponse_httpStatus' - The response's http status code.
newAssociateIamInstanceProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateIamInstanceProfileResponse
newAssociateIamInstanceProfileResponse pHttpStatus_ =
  AssociateIamInstanceProfileResponse'
    { iamInstanceProfileAssociation =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IAM instance profile association.
associateIamInstanceProfileResponse_iamInstanceProfileAssociation :: Lens.Lens' AssociateIamInstanceProfileResponse (Core.Maybe IamInstanceProfileAssociation)
associateIamInstanceProfileResponse_iamInstanceProfileAssociation = Lens.lens (\AssociateIamInstanceProfileResponse' {iamInstanceProfileAssociation} -> iamInstanceProfileAssociation) (\s@AssociateIamInstanceProfileResponse' {} a -> s {iamInstanceProfileAssociation = a} :: AssociateIamInstanceProfileResponse)

-- | The response's http status code.
associateIamInstanceProfileResponse_httpStatus :: Lens.Lens' AssociateIamInstanceProfileResponse Core.Int
associateIamInstanceProfileResponse_httpStatus = Lens.lens (\AssociateIamInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@AssociateIamInstanceProfileResponse' {} a -> s {httpStatus = a} :: AssociateIamInstanceProfileResponse)

instance
  Core.NFData
    AssociateIamInstanceProfileResponse
