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
-- Module      : Amazonka.EC2.ReplaceIamInstanceProfileAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an IAM instance profile for the specified running instance. You
-- can use this action to change the IAM instance profile that\'s
-- associated with an instance without having to disassociate the existing
-- IAM instance profile first.
--
-- Use DescribeIamInstanceProfileAssociations to get the association ID.
module Amazonka.EC2.ReplaceIamInstanceProfileAssociation
  ( -- * Creating a Request
    ReplaceIamInstanceProfileAssociation (..),
    newReplaceIamInstanceProfileAssociation,

    -- * Request Lenses
    replaceIamInstanceProfileAssociation_iamInstanceProfile,
    replaceIamInstanceProfileAssociation_associationId,

    -- * Destructuring the Response
    ReplaceIamInstanceProfileAssociationResponse (..),
    newReplaceIamInstanceProfileAssociationResponse,

    -- * Response Lenses
    replaceIamInstanceProfileAssociationResponse_iamInstanceProfileAssociation,
    replaceIamInstanceProfileAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReplaceIamInstanceProfileAssociation' smart constructor.
data ReplaceIamInstanceProfileAssociation = ReplaceIamInstanceProfileAssociation'
  { -- | The IAM instance profile.
    iamInstanceProfile :: IamInstanceProfileSpecification,
    -- | The ID of the existing IAM instance profile association.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceIamInstanceProfileAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamInstanceProfile', 'replaceIamInstanceProfileAssociation_iamInstanceProfile' - The IAM instance profile.
--
-- 'associationId', 'replaceIamInstanceProfileAssociation_associationId' - The ID of the existing IAM instance profile association.
newReplaceIamInstanceProfileAssociation ::
  -- | 'iamInstanceProfile'
  IamInstanceProfileSpecification ->
  -- | 'associationId'
  Prelude.Text ->
  ReplaceIamInstanceProfileAssociation
newReplaceIamInstanceProfileAssociation
  pIamInstanceProfile_
  pAssociationId_ =
    ReplaceIamInstanceProfileAssociation'
      { iamInstanceProfile =
          pIamInstanceProfile_,
        associationId = pAssociationId_
      }

-- | The IAM instance profile.
replaceIamInstanceProfileAssociation_iamInstanceProfile :: Lens.Lens' ReplaceIamInstanceProfileAssociation IamInstanceProfileSpecification
replaceIamInstanceProfileAssociation_iamInstanceProfile = Lens.lens (\ReplaceIamInstanceProfileAssociation' {iamInstanceProfile} -> iamInstanceProfile) (\s@ReplaceIamInstanceProfileAssociation' {} a -> s {iamInstanceProfile = a} :: ReplaceIamInstanceProfileAssociation)

-- | The ID of the existing IAM instance profile association.
replaceIamInstanceProfileAssociation_associationId :: Lens.Lens' ReplaceIamInstanceProfileAssociation Prelude.Text
replaceIamInstanceProfileAssociation_associationId = Lens.lens (\ReplaceIamInstanceProfileAssociation' {associationId} -> associationId) (\s@ReplaceIamInstanceProfileAssociation' {} a -> s {associationId = a} :: ReplaceIamInstanceProfileAssociation)

instance
  Core.AWSRequest
    ReplaceIamInstanceProfileAssociation
  where
  type
    AWSResponse ReplaceIamInstanceProfileAssociation =
      ReplaceIamInstanceProfileAssociationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ReplaceIamInstanceProfileAssociationResponse'
            Prelude.<$> (x Data..@? "iamInstanceProfileAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ReplaceIamInstanceProfileAssociation
  where
  hashWithSalt
    _salt
    ReplaceIamInstanceProfileAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` iamInstanceProfile
        `Prelude.hashWithSalt` associationId

instance
  Prelude.NFData
    ReplaceIamInstanceProfileAssociation
  where
  rnf ReplaceIamInstanceProfileAssociation' {..} =
    Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf associationId

instance
  Data.ToHeaders
    ReplaceIamInstanceProfileAssociation
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ReplaceIamInstanceProfileAssociation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ReplaceIamInstanceProfileAssociation
  where
  toQuery ReplaceIamInstanceProfileAssociation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ReplaceIamInstanceProfileAssociation" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "IamInstanceProfile" Data.=: iamInstanceProfile,
        "AssociationId" Data.=: associationId
      ]

-- | /See:/ 'newReplaceIamInstanceProfileAssociationResponse' smart constructor.
data ReplaceIamInstanceProfileAssociationResponse = ReplaceIamInstanceProfileAssociationResponse'
  { -- | Information about the IAM instance profile association.
    iamInstanceProfileAssociation :: Prelude.Maybe IamInstanceProfileAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceIamInstanceProfileAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamInstanceProfileAssociation', 'replaceIamInstanceProfileAssociationResponse_iamInstanceProfileAssociation' - Information about the IAM instance profile association.
--
-- 'httpStatus', 'replaceIamInstanceProfileAssociationResponse_httpStatus' - The response's http status code.
newReplaceIamInstanceProfileAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReplaceIamInstanceProfileAssociationResponse
newReplaceIamInstanceProfileAssociationResponse
  pHttpStatus_ =
    ReplaceIamInstanceProfileAssociationResponse'
      { iamInstanceProfileAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the IAM instance profile association.
replaceIamInstanceProfileAssociationResponse_iamInstanceProfileAssociation :: Lens.Lens' ReplaceIamInstanceProfileAssociationResponse (Prelude.Maybe IamInstanceProfileAssociation)
replaceIamInstanceProfileAssociationResponse_iamInstanceProfileAssociation = Lens.lens (\ReplaceIamInstanceProfileAssociationResponse' {iamInstanceProfileAssociation} -> iamInstanceProfileAssociation) (\s@ReplaceIamInstanceProfileAssociationResponse' {} a -> s {iamInstanceProfileAssociation = a} :: ReplaceIamInstanceProfileAssociationResponse)

-- | The response's http status code.
replaceIamInstanceProfileAssociationResponse_httpStatus :: Lens.Lens' ReplaceIamInstanceProfileAssociationResponse Prelude.Int
replaceIamInstanceProfileAssociationResponse_httpStatus = Lens.lens (\ReplaceIamInstanceProfileAssociationResponse' {httpStatus} -> httpStatus) (\s@ReplaceIamInstanceProfileAssociationResponse' {} a -> s {httpStatus = a} :: ReplaceIamInstanceProfileAssociationResponse)

instance
  Prelude.NFData
    ReplaceIamInstanceProfileAssociationResponse
  where
  rnf ReplaceIamInstanceProfileAssociationResponse' {..} =
    Prelude.rnf iamInstanceProfileAssociation
      `Prelude.seq` Prelude.rnf httpStatus
