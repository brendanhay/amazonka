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
-- Module      : Amazonka.EC2.AssociateIamInstanceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an IAM instance profile with a running or stopped instance.
-- You cannot associate more than one IAM instance profile with an
-- instance.
module Amazonka.EC2.AssociateIamInstanceProfile
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateIamInstanceProfile' smart constructor.
data AssociateIamInstanceProfile = AssociateIamInstanceProfile'
  { -- | The IAM instance profile.
    iamInstanceProfile :: IamInstanceProfileSpecification,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
associateIamInstanceProfile_instanceId :: Lens.Lens' AssociateIamInstanceProfile Prelude.Text
associateIamInstanceProfile_instanceId = Lens.lens (\AssociateIamInstanceProfile' {instanceId} -> instanceId) (\s@AssociateIamInstanceProfile' {} a -> s {instanceId = a} :: AssociateIamInstanceProfile)

instance Core.AWSRequest AssociateIamInstanceProfile where
  type
    AWSResponse AssociateIamInstanceProfile =
      AssociateIamInstanceProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateIamInstanceProfileResponse'
            Prelude.<$> (x Data..@? "iamInstanceProfileAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateIamInstanceProfile where
  hashWithSalt _salt AssociateIamInstanceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` iamInstanceProfile
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData AssociateIamInstanceProfile where
  rnf AssociateIamInstanceProfile' {..} =
    Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders AssociateIamInstanceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateIamInstanceProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateIamInstanceProfile where
  toQuery AssociateIamInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AssociateIamInstanceProfile" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "IamInstanceProfile" Data.=: iamInstanceProfile,
        "InstanceId" Data.=: instanceId
      ]

-- | /See:/ 'newAssociateIamInstanceProfileResponse' smart constructor.
data AssociateIamInstanceProfileResponse = AssociateIamInstanceProfileResponse'
  { -- | Information about the IAM instance profile association.
    iamInstanceProfileAssociation :: Prelude.Maybe IamInstanceProfileAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AssociateIamInstanceProfileResponse
newAssociateIamInstanceProfileResponse pHttpStatus_ =
  AssociateIamInstanceProfileResponse'
    { iamInstanceProfileAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IAM instance profile association.
associateIamInstanceProfileResponse_iamInstanceProfileAssociation :: Lens.Lens' AssociateIamInstanceProfileResponse (Prelude.Maybe IamInstanceProfileAssociation)
associateIamInstanceProfileResponse_iamInstanceProfileAssociation = Lens.lens (\AssociateIamInstanceProfileResponse' {iamInstanceProfileAssociation} -> iamInstanceProfileAssociation) (\s@AssociateIamInstanceProfileResponse' {} a -> s {iamInstanceProfileAssociation = a} :: AssociateIamInstanceProfileResponse)

-- | The response's http status code.
associateIamInstanceProfileResponse_httpStatus :: Lens.Lens' AssociateIamInstanceProfileResponse Prelude.Int
associateIamInstanceProfileResponse_httpStatus = Lens.lens (\AssociateIamInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@AssociateIamInstanceProfileResponse' {} a -> s {httpStatus = a} :: AssociateIamInstanceProfileResponse)

instance
  Prelude.NFData
    AssociateIamInstanceProfileResponse
  where
  rnf AssociateIamInstanceProfileResponse' {..} =
    Prelude.rnf iamInstanceProfileAssociation
      `Prelude.seq` Prelude.rnf httpStatus
