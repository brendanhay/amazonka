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
-- Module      : Amazonka.IoT.CreateAuditSuppression
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Device Defender audit suppression.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateAuditSuppression>
-- action.
module Amazonka.IoT.CreateAuditSuppression
  ( -- * Creating a Request
    CreateAuditSuppression (..),
    newCreateAuditSuppression,

    -- * Request Lenses
    createAuditSuppression_description,
    createAuditSuppression_expirationDate,
    createAuditSuppression_suppressIndefinitely,
    createAuditSuppression_checkName,
    createAuditSuppression_resourceIdentifier,
    createAuditSuppression_clientRequestToken,

    -- * Destructuring the Response
    CreateAuditSuppressionResponse (..),
    newCreateAuditSuppressionResponse,

    -- * Response Lenses
    createAuditSuppressionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAuditSuppression' smart constructor.
data CreateAuditSuppression = CreateAuditSuppression'
  { -- | The description of the audit suppression.
    description :: Prelude.Maybe Prelude.Text,
    -- | The epoch timestamp in seconds at which this suppression expires.
    expirationDate :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Prelude.Maybe Prelude.Bool,
    checkName :: Prelude.Text,
    resourceIdentifier :: ResourceIdentifier,
    -- | Each audit supression must have a unique client request token. If you
    -- try to create a new audit suppression with the same token as one that
    -- already exists, an exception occurs. If you omit this value, Amazon Web
    -- Services SDKs will automatically generate a unique client request.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAuditSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createAuditSuppression_description' - The description of the audit suppression.
--
-- 'expirationDate', 'createAuditSuppression_expirationDate' - The epoch timestamp in seconds at which this suppression expires.
--
-- 'suppressIndefinitely', 'createAuditSuppression_suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- 'checkName', 'createAuditSuppression_checkName' - Undocumented member.
--
-- 'resourceIdentifier', 'createAuditSuppression_resourceIdentifier' - Undocumented member.
--
-- 'clientRequestToken', 'createAuditSuppression_clientRequestToken' - Each audit supression must have a unique client request token. If you
-- try to create a new audit suppression with the same token as one that
-- already exists, an exception occurs. If you omit this value, Amazon Web
-- Services SDKs will automatically generate a unique client request.
newCreateAuditSuppression ::
  -- | 'checkName'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateAuditSuppression
newCreateAuditSuppression
  pCheckName_
  pResourceIdentifier_
  pClientRequestToken_ =
    CreateAuditSuppression'
      { description =
          Prelude.Nothing,
        expirationDate = Prelude.Nothing,
        suppressIndefinitely = Prelude.Nothing,
        checkName = pCheckName_,
        resourceIdentifier = pResourceIdentifier_,
        clientRequestToken = pClientRequestToken_
      }

-- | The description of the audit suppression.
createAuditSuppression_description :: Lens.Lens' CreateAuditSuppression (Prelude.Maybe Prelude.Text)
createAuditSuppression_description = Lens.lens (\CreateAuditSuppression' {description} -> description) (\s@CreateAuditSuppression' {} a -> s {description = a} :: CreateAuditSuppression)

-- | The epoch timestamp in seconds at which this suppression expires.
createAuditSuppression_expirationDate :: Lens.Lens' CreateAuditSuppression (Prelude.Maybe Prelude.UTCTime)
createAuditSuppression_expirationDate = Lens.lens (\CreateAuditSuppression' {expirationDate} -> expirationDate) (\s@CreateAuditSuppression' {} a -> s {expirationDate = a} :: CreateAuditSuppression) Prelude.. Lens.mapping Data._Time

-- | Indicates whether a suppression should exist indefinitely or not.
createAuditSuppression_suppressIndefinitely :: Lens.Lens' CreateAuditSuppression (Prelude.Maybe Prelude.Bool)
createAuditSuppression_suppressIndefinitely = Lens.lens (\CreateAuditSuppression' {suppressIndefinitely} -> suppressIndefinitely) (\s@CreateAuditSuppression' {} a -> s {suppressIndefinitely = a} :: CreateAuditSuppression)

-- | Undocumented member.
createAuditSuppression_checkName :: Lens.Lens' CreateAuditSuppression Prelude.Text
createAuditSuppression_checkName = Lens.lens (\CreateAuditSuppression' {checkName} -> checkName) (\s@CreateAuditSuppression' {} a -> s {checkName = a} :: CreateAuditSuppression)

-- | Undocumented member.
createAuditSuppression_resourceIdentifier :: Lens.Lens' CreateAuditSuppression ResourceIdentifier
createAuditSuppression_resourceIdentifier = Lens.lens (\CreateAuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@CreateAuditSuppression' {} a -> s {resourceIdentifier = a} :: CreateAuditSuppression)

-- | Each audit supression must have a unique client request token. If you
-- try to create a new audit suppression with the same token as one that
-- already exists, an exception occurs. If you omit this value, Amazon Web
-- Services SDKs will automatically generate a unique client request.
createAuditSuppression_clientRequestToken :: Lens.Lens' CreateAuditSuppression Prelude.Text
createAuditSuppression_clientRequestToken = Lens.lens (\CreateAuditSuppression' {clientRequestToken} -> clientRequestToken) (\s@CreateAuditSuppression' {} a -> s {clientRequestToken = a} :: CreateAuditSuppression)

instance Core.AWSRequest CreateAuditSuppression where
  type
    AWSResponse CreateAuditSuppression =
      CreateAuditSuppressionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateAuditSuppressionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAuditSuppression where
  hashWithSalt _salt CreateAuditSuppression' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expirationDate
      `Prelude.hashWithSalt` suppressIndefinitely
      `Prelude.hashWithSalt` checkName
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateAuditSuppression where
  rnf CreateAuditSuppression' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf suppressIndefinitely
      `Prelude.seq` Prelude.rnf checkName
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateAuditSuppression where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateAuditSuppression where
  toJSON CreateAuditSuppression' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("expirationDate" Data..=)
              Prelude.<$> expirationDate,
            ("suppressIndefinitely" Data..=)
              Prelude.<$> suppressIndefinitely,
            Prelude.Just ("checkName" Data..= checkName),
            Prelude.Just
              ("resourceIdentifier" Data..= resourceIdentifier),
            Prelude.Just
              ("clientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateAuditSuppression where
  toPath = Prelude.const "/audit/suppressions/create"

instance Data.ToQuery CreateAuditSuppression where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAuditSuppressionResponse' smart constructor.
data CreateAuditSuppressionResponse = CreateAuditSuppressionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAuditSuppressionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAuditSuppressionResponse_httpStatus' - The response's http status code.
newCreateAuditSuppressionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAuditSuppressionResponse
newCreateAuditSuppressionResponse pHttpStatus_ =
  CreateAuditSuppressionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createAuditSuppressionResponse_httpStatus :: Lens.Lens' CreateAuditSuppressionResponse Prelude.Int
createAuditSuppressionResponse_httpStatus = Lens.lens (\CreateAuditSuppressionResponse' {httpStatus} -> httpStatus) (\s@CreateAuditSuppressionResponse' {} a -> s {httpStatus = a} :: CreateAuditSuppressionResponse)

instance
  Prelude.NFData
    CreateAuditSuppressionResponse
  where
  rnf CreateAuditSuppressionResponse' {..} =
    Prelude.rnf httpStatus
