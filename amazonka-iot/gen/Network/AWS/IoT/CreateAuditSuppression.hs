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
-- Module      : Network.AWS.IoT.CreateAuditSuppression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Device Defender audit suppression.
module Network.AWS.IoT.CreateAuditSuppression
  ( -- * Creating a Request
    CreateAuditSuppression (..),
    newCreateAuditSuppression,

    -- * Request Lenses
    createAuditSuppression_expirationDate,
    createAuditSuppression_description,
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAuditSuppression' smart constructor.
data CreateAuditSuppression = CreateAuditSuppression'
  { -- | The epoch timestamp in seconds at which this suppression expires.
    expirationDate :: Core.Maybe Core.POSIX,
    -- | The description of the audit suppression.
    description :: Core.Maybe Core.Text,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Core.Maybe Core.Bool,
    checkName :: Core.Text,
    resourceIdentifier :: ResourceIdentifier,
    -- | The epoch timestamp in seconds at which this suppression expires.
    clientRequestToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAuditSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationDate', 'createAuditSuppression_expirationDate' - The epoch timestamp in seconds at which this suppression expires.
--
-- 'description', 'createAuditSuppression_description' - The description of the audit suppression.
--
-- 'suppressIndefinitely', 'createAuditSuppression_suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- 'checkName', 'createAuditSuppression_checkName' - Undocumented member.
--
-- 'resourceIdentifier', 'createAuditSuppression_resourceIdentifier' - Undocumented member.
--
-- 'clientRequestToken', 'createAuditSuppression_clientRequestToken' - The epoch timestamp in seconds at which this suppression expires.
newCreateAuditSuppression ::
  -- | 'checkName'
  Core.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  -- | 'clientRequestToken'
  Core.Text ->
  CreateAuditSuppression
newCreateAuditSuppression
  pCheckName_
  pResourceIdentifier_
  pClientRequestToken_ =
    CreateAuditSuppression'
      { expirationDate =
          Core.Nothing,
        description = Core.Nothing,
        suppressIndefinitely = Core.Nothing,
        checkName = pCheckName_,
        resourceIdentifier = pResourceIdentifier_,
        clientRequestToken = pClientRequestToken_
      }

-- | The epoch timestamp in seconds at which this suppression expires.
createAuditSuppression_expirationDate :: Lens.Lens' CreateAuditSuppression (Core.Maybe Core.UTCTime)
createAuditSuppression_expirationDate = Lens.lens (\CreateAuditSuppression' {expirationDate} -> expirationDate) (\s@CreateAuditSuppression' {} a -> s {expirationDate = a} :: CreateAuditSuppression) Core.. Lens.mapping Core._Time

-- | The description of the audit suppression.
createAuditSuppression_description :: Lens.Lens' CreateAuditSuppression (Core.Maybe Core.Text)
createAuditSuppression_description = Lens.lens (\CreateAuditSuppression' {description} -> description) (\s@CreateAuditSuppression' {} a -> s {description = a} :: CreateAuditSuppression)

-- | Indicates whether a suppression should exist indefinitely or not.
createAuditSuppression_suppressIndefinitely :: Lens.Lens' CreateAuditSuppression (Core.Maybe Core.Bool)
createAuditSuppression_suppressIndefinitely = Lens.lens (\CreateAuditSuppression' {suppressIndefinitely} -> suppressIndefinitely) (\s@CreateAuditSuppression' {} a -> s {suppressIndefinitely = a} :: CreateAuditSuppression)

-- | Undocumented member.
createAuditSuppression_checkName :: Lens.Lens' CreateAuditSuppression Core.Text
createAuditSuppression_checkName = Lens.lens (\CreateAuditSuppression' {checkName} -> checkName) (\s@CreateAuditSuppression' {} a -> s {checkName = a} :: CreateAuditSuppression)

-- | Undocumented member.
createAuditSuppression_resourceIdentifier :: Lens.Lens' CreateAuditSuppression ResourceIdentifier
createAuditSuppression_resourceIdentifier = Lens.lens (\CreateAuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@CreateAuditSuppression' {} a -> s {resourceIdentifier = a} :: CreateAuditSuppression)

-- | The epoch timestamp in seconds at which this suppression expires.
createAuditSuppression_clientRequestToken :: Lens.Lens' CreateAuditSuppression Core.Text
createAuditSuppression_clientRequestToken = Lens.lens (\CreateAuditSuppression' {clientRequestToken} -> clientRequestToken) (\s@CreateAuditSuppression' {} a -> s {clientRequestToken = a} :: CreateAuditSuppression)

instance Core.AWSRequest CreateAuditSuppression where
  type
    AWSResponse CreateAuditSuppression =
      CreateAuditSuppressionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateAuditSuppressionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateAuditSuppression

instance Core.NFData CreateAuditSuppression

instance Core.ToHeaders CreateAuditSuppression where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateAuditSuppression where
  toJSON CreateAuditSuppression' {..} =
    Core.object
      ( Core.catMaybes
          [ ("expirationDate" Core..=) Core.<$> expirationDate,
            ("description" Core..=) Core.<$> description,
            ("suppressIndefinitely" Core..=)
              Core.<$> suppressIndefinitely,
            Core.Just ("checkName" Core..= checkName),
            Core.Just
              ("resourceIdentifier" Core..= resourceIdentifier),
            Core.Just
              ("clientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath CreateAuditSuppression where
  toPath = Core.const "/audit/suppressions/create"

instance Core.ToQuery CreateAuditSuppression where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateAuditSuppressionResponse' smart constructor.
data CreateAuditSuppressionResponse = CreateAuditSuppressionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateAuditSuppressionResponse
newCreateAuditSuppressionResponse pHttpStatus_ =
  CreateAuditSuppressionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createAuditSuppressionResponse_httpStatus :: Lens.Lens' CreateAuditSuppressionResponse Core.Int
createAuditSuppressionResponse_httpStatus = Lens.lens (\CreateAuditSuppressionResponse' {httpStatus} -> httpStatus) (\s@CreateAuditSuppressionResponse' {} a -> s {httpStatus = a} :: CreateAuditSuppressionResponse)

instance Core.NFData CreateAuditSuppressionResponse
