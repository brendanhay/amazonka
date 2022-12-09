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
-- Module      : Amazonka.EC2.CreateVerifiedAccessInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An Amazon Web Services Verified Access instance is a regional entity
-- that evaluates application requests and grants access only when your
-- security requirements are met.
module Amazonka.EC2.CreateVerifiedAccessInstance
  ( -- * Creating a Request
    CreateVerifiedAccessInstance (..),
    newCreateVerifiedAccessInstance,

    -- * Request Lenses
    createVerifiedAccessInstance_clientToken,
    createVerifiedAccessInstance_description,
    createVerifiedAccessInstance_dryRun,
    createVerifiedAccessInstance_tagSpecifications,

    -- * Destructuring the Response
    CreateVerifiedAccessInstanceResponse (..),
    newCreateVerifiedAccessInstanceResponse,

    -- * Response Lenses
    createVerifiedAccessInstanceResponse_verifiedAccessInstance,
    createVerifiedAccessInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVerifiedAccessInstance' smart constructor.
data CreateVerifiedAccessInstance = CreateVerifiedAccessInstance'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access instance.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to the Amazon Web Services Verified Access instance.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createVerifiedAccessInstance_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'createVerifiedAccessInstance_description' - A description for the Amazon Web Services Verified Access instance.
--
-- 'dryRun', 'createVerifiedAccessInstance_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createVerifiedAccessInstance_tagSpecifications' - The tags to assign to the Amazon Web Services Verified Access instance.
newCreateVerifiedAccessInstance ::
  CreateVerifiedAccessInstance
newCreateVerifiedAccessInstance =
  CreateVerifiedAccessInstance'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
createVerifiedAccessInstance_clientToken :: Lens.Lens' CreateVerifiedAccessInstance (Prelude.Maybe Prelude.Text)
createVerifiedAccessInstance_clientToken = Lens.lens (\CreateVerifiedAccessInstance' {clientToken} -> clientToken) (\s@CreateVerifiedAccessInstance' {} a -> s {clientToken = a} :: CreateVerifiedAccessInstance)

-- | A description for the Amazon Web Services Verified Access instance.
createVerifiedAccessInstance_description :: Lens.Lens' CreateVerifiedAccessInstance (Prelude.Maybe Prelude.Text)
createVerifiedAccessInstance_description = Lens.lens (\CreateVerifiedAccessInstance' {description} -> description) (\s@CreateVerifiedAccessInstance' {} a -> s {description = a} :: CreateVerifiedAccessInstance)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVerifiedAccessInstance_dryRun :: Lens.Lens' CreateVerifiedAccessInstance (Prelude.Maybe Prelude.Bool)
createVerifiedAccessInstance_dryRun = Lens.lens (\CreateVerifiedAccessInstance' {dryRun} -> dryRun) (\s@CreateVerifiedAccessInstance' {} a -> s {dryRun = a} :: CreateVerifiedAccessInstance)

-- | The tags to assign to the Amazon Web Services Verified Access instance.
createVerifiedAccessInstance_tagSpecifications :: Lens.Lens' CreateVerifiedAccessInstance (Prelude.Maybe [TagSpecification])
createVerifiedAccessInstance_tagSpecifications = Lens.lens (\CreateVerifiedAccessInstance' {tagSpecifications} -> tagSpecifications) (\s@CreateVerifiedAccessInstance' {} a -> s {tagSpecifications = a} :: CreateVerifiedAccessInstance) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateVerifiedAccessInstance where
  type
    AWSResponse CreateVerifiedAccessInstance =
      CreateVerifiedAccessInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVerifiedAccessInstanceResponse'
            Prelude.<$> (x Data..@? "verifiedAccessInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateVerifiedAccessInstance
  where
  hashWithSalt _salt CreateVerifiedAccessInstance' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications

instance Prelude.NFData CreateVerifiedAccessInstance where
  rnf CreateVerifiedAccessInstance' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications

instance Data.ToHeaders CreateVerifiedAccessInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateVerifiedAccessInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVerifiedAccessInstance where
  toQuery CreateVerifiedAccessInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateVerifiedAccessInstance" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          )
      ]

-- | /See:/ 'newCreateVerifiedAccessInstanceResponse' smart constructor.
data CreateVerifiedAccessInstanceResponse = CreateVerifiedAccessInstanceResponse'
  { -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstance :: Prelude.Maybe VerifiedAccessInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessInstance', 'createVerifiedAccessInstanceResponse_verifiedAccessInstance' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'httpStatus', 'createVerifiedAccessInstanceResponse_httpStatus' - The response's http status code.
newCreateVerifiedAccessInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVerifiedAccessInstanceResponse
newCreateVerifiedAccessInstanceResponse pHttpStatus_ =
  CreateVerifiedAccessInstanceResponse'
    { verifiedAccessInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Amazon Web Services Verified Access instance.
createVerifiedAccessInstanceResponse_verifiedAccessInstance :: Lens.Lens' CreateVerifiedAccessInstanceResponse (Prelude.Maybe VerifiedAccessInstance)
createVerifiedAccessInstanceResponse_verifiedAccessInstance = Lens.lens (\CreateVerifiedAccessInstanceResponse' {verifiedAccessInstance} -> verifiedAccessInstance) (\s@CreateVerifiedAccessInstanceResponse' {} a -> s {verifiedAccessInstance = a} :: CreateVerifiedAccessInstanceResponse)

-- | The response's http status code.
createVerifiedAccessInstanceResponse_httpStatus :: Lens.Lens' CreateVerifiedAccessInstanceResponse Prelude.Int
createVerifiedAccessInstanceResponse_httpStatus = Lens.lens (\CreateVerifiedAccessInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateVerifiedAccessInstanceResponse' {} a -> s {httpStatus = a} :: CreateVerifiedAccessInstanceResponse)

instance
  Prelude.NFData
    CreateVerifiedAccessInstanceResponse
  where
  rnf CreateVerifiedAccessInstanceResponse' {..} =
    Prelude.rnf verifiedAccessInstance
      `Prelude.seq` Prelude.rnf httpStatus
