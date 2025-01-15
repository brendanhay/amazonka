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
-- Module      : Amazonka.EC2.CreateVerifiedAccessGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An Amazon Web Services Verified Access group is a collection of Amazon
-- Web Services Verified Access endpoints who\'s associated applications
-- have similar security requirements. Each instance within an Amazon Web
-- Services Verified Access group shares an Amazon Web Services Verified
-- Access policy. For example, you can group all Amazon Web Services
-- Verified Access instances associated with “sales” applications together
-- and use one common Amazon Web Services Verified Access policy.
module Amazonka.EC2.CreateVerifiedAccessGroup
  ( -- * Creating a Request
    CreateVerifiedAccessGroup (..),
    newCreateVerifiedAccessGroup,

    -- * Request Lenses
    createVerifiedAccessGroup_clientToken,
    createVerifiedAccessGroup_description,
    createVerifiedAccessGroup_dryRun,
    createVerifiedAccessGroup_policyDocument,
    createVerifiedAccessGroup_tagSpecifications,
    createVerifiedAccessGroup_verifiedAccessInstanceId,

    -- * Destructuring the Response
    CreateVerifiedAccessGroupResponse (..),
    newCreateVerifiedAccessGroupResponse,

    -- * Response Lenses
    createVerifiedAccessGroupResponse_verifiedAccessGroup,
    createVerifiedAccessGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVerifiedAccessGroup' smart constructor.
data CreateVerifiedAccessGroup = CreateVerifiedAccessGroup'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access group.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services Verified Access policy document.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the Amazon Web Services Verified Access group.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createVerifiedAccessGroup_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'createVerifiedAccessGroup_description' - A description for the Amazon Web Services Verified Access group.
--
-- 'dryRun', 'createVerifiedAccessGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'policyDocument', 'createVerifiedAccessGroup_policyDocument' - The Amazon Web Services Verified Access policy document.
--
-- 'tagSpecifications', 'createVerifiedAccessGroup_tagSpecifications' - The tags to assign to the Amazon Web Services Verified Access group.
--
-- 'verifiedAccessInstanceId', 'createVerifiedAccessGroup_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
newCreateVerifiedAccessGroup ::
  -- | 'verifiedAccessInstanceId'
  Prelude.Text ->
  CreateVerifiedAccessGroup
newCreateVerifiedAccessGroup
  pVerifiedAccessInstanceId_ =
    CreateVerifiedAccessGroup'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        policyDocument = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        verifiedAccessInstanceId =
          pVerifiedAccessInstanceId_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
createVerifiedAccessGroup_clientToken :: Lens.Lens' CreateVerifiedAccessGroup (Prelude.Maybe Prelude.Text)
createVerifiedAccessGroup_clientToken = Lens.lens (\CreateVerifiedAccessGroup' {clientToken} -> clientToken) (\s@CreateVerifiedAccessGroup' {} a -> s {clientToken = a} :: CreateVerifiedAccessGroup)

-- | A description for the Amazon Web Services Verified Access group.
createVerifiedAccessGroup_description :: Lens.Lens' CreateVerifiedAccessGroup (Prelude.Maybe Prelude.Text)
createVerifiedAccessGroup_description = Lens.lens (\CreateVerifiedAccessGroup' {description} -> description) (\s@CreateVerifiedAccessGroup' {} a -> s {description = a} :: CreateVerifiedAccessGroup)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVerifiedAccessGroup_dryRun :: Lens.Lens' CreateVerifiedAccessGroup (Prelude.Maybe Prelude.Bool)
createVerifiedAccessGroup_dryRun = Lens.lens (\CreateVerifiedAccessGroup' {dryRun} -> dryRun) (\s@CreateVerifiedAccessGroup' {} a -> s {dryRun = a} :: CreateVerifiedAccessGroup)

-- | The Amazon Web Services Verified Access policy document.
createVerifiedAccessGroup_policyDocument :: Lens.Lens' CreateVerifiedAccessGroup (Prelude.Maybe Prelude.Text)
createVerifiedAccessGroup_policyDocument = Lens.lens (\CreateVerifiedAccessGroup' {policyDocument} -> policyDocument) (\s@CreateVerifiedAccessGroup' {} a -> s {policyDocument = a} :: CreateVerifiedAccessGroup)

-- | The tags to assign to the Amazon Web Services Verified Access group.
createVerifiedAccessGroup_tagSpecifications :: Lens.Lens' CreateVerifiedAccessGroup (Prelude.Maybe [TagSpecification])
createVerifiedAccessGroup_tagSpecifications = Lens.lens (\CreateVerifiedAccessGroup' {tagSpecifications} -> tagSpecifications) (\s@CreateVerifiedAccessGroup' {} a -> s {tagSpecifications = a} :: CreateVerifiedAccessGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services Verified Access instance.
createVerifiedAccessGroup_verifiedAccessInstanceId :: Lens.Lens' CreateVerifiedAccessGroup Prelude.Text
createVerifiedAccessGroup_verifiedAccessInstanceId = Lens.lens (\CreateVerifiedAccessGroup' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@CreateVerifiedAccessGroup' {} a -> s {verifiedAccessInstanceId = a} :: CreateVerifiedAccessGroup)

instance Core.AWSRequest CreateVerifiedAccessGroup where
  type
    AWSResponse CreateVerifiedAccessGroup =
      CreateVerifiedAccessGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVerifiedAccessGroupResponse'
            Prelude.<$> (x Data..@? "verifiedAccessGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVerifiedAccessGroup where
  hashWithSalt _salt CreateVerifiedAccessGroup' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` verifiedAccessInstanceId

instance Prelude.NFData CreateVerifiedAccessGroup where
  rnf CreateVerifiedAccessGroup' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf dryRun `Prelude.seq`
          Prelude.rnf policyDocument `Prelude.seq`
            Prelude.rnf tagSpecifications `Prelude.seq`
              Prelude.rnf verifiedAccessInstanceId

instance Data.ToHeaders CreateVerifiedAccessGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateVerifiedAccessGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVerifiedAccessGroup where
  toQuery CreateVerifiedAccessGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateVerifiedAccessGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "PolicyDocument" Data.=: policyDocument,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "VerifiedAccessInstanceId"
          Data.=: verifiedAccessInstanceId
      ]

-- | /See:/ 'newCreateVerifiedAccessGroupResponse' smart constructor.
data CreateVerifiedAccessGroupResponse = CreateVerifiedAccessGroupResponse'
  { -- | The ID of the Verified Access group.
    verifiedAccessGroup :: Prelude.Maybe VerifiedAccessGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessGroup', 'createVerifiedAccessGroupResponse_verifiedAccessGroup' - The ID of the Verified Access group.
--
-- 'httpStatus', 'createVerifiedAccessGroupResponse_httpStatus' - The response's http status code.
newCreateVerifiedAccessGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVerifiedAccessGroupResponse
newCreateVerifiedAccessGroupResponse pHttpStatus_ =
  CreateVerifiedAccessGroupResponse'
    { verifiedAccessGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Verified Access group.
createVerifiedAccessGroupResponse_verifiedAccessGroup :: Lens.Lens' CreateVerifiedAccessGroupResponse (Prelude.Maybe VerifiedAccessGroup)
createVerifiedAccessGroupResponse_verifiedAccessGroup = Lens.lens (\CreateVerifiedAccessGroupResponse' {verifiedAccessGroup} -> verifiedAccessGroup) (\s@CreateVerifiedAccessGroupResponse' {} a -> s {verifiedAccessGroup = a} :: CreateVerifiedAccessGroupResponse)

-- | The response's http status code.
createVerifiedAccessGroupResponse_httpStatus :: Lens.Lens' CreateVerifiedAccessGroupResponse Prelude.Int
createVerifiedAccessGroupResponse_httpStatus = Lens.lens (\CreateVerifiedAccessGroupResponse' {httpStatus} -> httpStatus) (\s@CreateVerifiedAccessGroupResponse' {} a -> s {httpStatus = a} :: CreateVerifiedAccessGroupResponse)

instance
  Prelude.NFData
    CreateVerifiedAccessGroupResponse
  where
  rnf CreateVerifiedAccessGroupResponse' {..} =
    Prelude.rnf verifiedAccessGroup `Prelude.seq`
      Prelude.rnf httpStatus
