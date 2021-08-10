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
-- Module      : Network.AWS.EC2.DisassociateIamInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an IAM instance profile from a running or stopped
-- instance.
--
-- Use DescribeIamInstanceProfileAssociations to get the association ID.
module Network.AWS.EC2.DisassociateIamInstanceProfile
  ( -- * Creating a Request
    DisassociateIamInstanceProfile (..),
    newDisassociateIamInstanceProfile,

    -- * Request Lenses
    disassociateIamInstanceProfile_associationId,

    -- * Destructuring the Response
    DisassociateIamInstanceProfileResponse (..),
    newDisassociateIamInstanceProfileResponse,

    -- * Response Lenses
    disassociateIamInstanceProfileResponse_iamInstanceProfileAssociation,
    disassociateIamInstanceProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateIamInstanceProfile' smart constructor.
data DisassociateIamInstanceProfile = DisassociateIamInstanceProfile'
  { -- | The ID of the IAM instance profile association.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateIamInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'disassociateIamInstanceProfile_associationId' - The ID of the IAM instance profile association.
newDisassociateIamInstanceProfile ::
  -- | 'associationId'
  Prelude.Text ->
  DisassociateIamInstanceProfile
newDisassociateIamInstanceProfile pAssociationId_ =
  DisassociateIamInstanceProfile'
    { associationId =
        pAssociationId_
    }

-- | The ID of the IAM instance profile association.
disassociateIamInstanceProfile_associationId :: Lens.Lens' DisassociateIamInstanceProfile Prelude.Text
disassociateIamInstanceProfile_associationId = Lens.lens (\DisassociateIamInstanceProfile' {associationId} -> associationId) (\s@DisassociateIamInstanceProfile' {} a -> s {associationId = a} :: DisassociateIamInstanceProfile)

instance
  Core.AWSRequest
    DisassociateIamInstanceProfile
  where
  type
    AWSResponse DisassociateIamInstanceProfile =
      DisassociateIamInstanceProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateIamInstanceProfileResponse'
            Prelude.<$> (x Core..@? "iamInstanceProfileAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateIamInstanceProfile

instance
  Prelude.NFData
    DisassociateIamInstanceProfile

instance
  Core.ToHeaders
    DisassociateIamInstanceProfile
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DisassociateIamInstanceProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery DisassociateIamInstanceProfile where
  toQuery DisassociateIamInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DisassociateIamInstanceProfile" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "AssociationId" Core.=: associationId
      ]

-- | /See:/ 'newDisassociateIamInstanceProfileResponse' smart constructor.
data DisassociateIamInstanceProfileResponse = DisassociateIamInstanceProfileResponse'
  { -- | Information about the IAM instance profile association.
    iamInstanceProfileAssociation :: Prelude.Maybe IamInstanceProfileAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateIamInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamInstanceProfileAssociation', 'disassociateIamInstanceProfileResponse_iamInstanceProfileAssociation' - Information about the IAM instance profile association.
--
-- 'httpStatus', 'disassociateIamInstanceProfileResponse_httpStatus' - The response's http status code.
newDisassociateIamInstanceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateIamInstanceProfileResponse
newDisassociateIamInstanceProfileResponse
  pHttpStatus_ =
    DisassociateIamInstanceProfileResponse'
      { iamInstanceProfileAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the IAM instance profile association.
disassociateIamInstanceProfileResponse_iamInstanceProfileAssociation :: Lens.Lens' DisassociateIamInstanceProfileResponse (Prelude.Maybe IamInstanceProfileAssociation)
disassociateIamInstanceProfileResponse_iamInstanceProfileAssociation = Lens.lens (\DisassociateIamInstanceProfileResponse' {iamInstanceProfileAssociation} -> iamInstanceProfileAssociation) (\s@DisassociateIamInstanceProfileResponse' {} a -> s {iamInstanceProfileAssociation = a} :: DisassociateIamInstanceProfileResponse)

-- | The response's http status code.
disassociateIamInstanceProfileResponse_httpStatus :: Lens.Lens' DisassociateIamInstanceProfileResponse Prelude.Int
disassociateIamInstanceProfileResponse_httpStatus = Lens.lens (\DisassociateIamInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@DisassociateIamInstanceProfileResponse' {} a -> s {httpStatus = a} :: DisassociateIamInstanceProfileResponse)

instance
  Prelude.NFData
    DisassociateIamInstanceProfileResponse
