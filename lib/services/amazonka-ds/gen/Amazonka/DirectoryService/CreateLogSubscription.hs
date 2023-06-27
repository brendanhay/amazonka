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
-- Module      : Amazonka.DirectoryService.CreateLogSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription to forward real-time Directory Service domain
-- controller security logs to the specified Amazon CloudWatch log group in
-- your Amazon Web Services account.
module Amazonka.DirectoryService.CreateLogSubscription
  ( -- * Creating a Request
    CreateLogSubscription (..),
    newCreateLogSubscription,

    -- * Request Lenses
    createLogSubscription_directoryId,
    createLogSubscription_logGroupName,

    -- * Destructuring the Response
    CreateLogSubscriptionResponse (..),
    newCreateLogSubscriptionResponse,

    -- * Response Lenses
    createLogSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLogSubscription' smart constructor.
data CreateLogSubscription = CreateLogSubscription'
  { -- | Identifier of the directory to which you want to subscribe and receive
    -- real-time logs to your specified CloudWatch log group.
    directoryId :: Prelude.Text,
    -- | The name of the CloudWatch log group where the real-time domain
    -- controller logs are forwarded.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLogSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'createLogSubscription_directoryId' - Identifier of the directory to which you want to subscribe and receive
-- real-time logs to your specified CloudWatch log group.
--
-- 'logGroupName', 'createLogSubscription_logGroupName' - The name of the CloudWatch log group where the real-time domain
-- controller logs are forwarded.
newCreateLogSubscription ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'logGroupName'
  Prelude.Text ->
  CreateLogSubscription
newCreateLogSubscription pDirectoryId_ pLogGroupName_ =
  CreateLogSubscription'
    { directoryId = pDirectoryId_,
      logGroupName = pLogGroupName_
    }

-- | Identifier of the directory to which you want to subscribe and receive
-- real-time logs to your specified CloudWatch log group.
createLogSubscription_directoryId :: Lens.Lens' CreateLogSubscription Prelude.Text
createLogSubscription_directoryId = Lens.lens (\CreateLogSubscription' {directoryId} -> directoryId) (\s@CreateLogSubscription' {} a -> s {directoryId = a} :: CreateLogSubscription)

-- | The name of the CloudWatch log group where the real-time domain
-- controller logs are forwarded.
createLogSubscription_logGroupName :: Lens.Lens' CreateLogSubscription Prelude.Text
createLogSubscription_logGroupName = Lens.lens (\CreateLogSubscription' {logGroupName} -> logGroupName) (\s@CreateLogSubscription' {} a -> s {logGroupName = a} :: CreateLogSubscription)

instance Core.AWSRequest CreateLogSubscription where
  type
    AWSResponse CreateLogSubscription =
      CreateLogSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateLogSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLogSubscription where
  hashWithSalt _salt CreateLogSubscription' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData CreateLogSubscription where
  rnf CreateLogSubscription' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf logGroupName

instance Data.ToHeaders CreateLogSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.CreateLogSubscription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLogSubscription where
  toJSON CreateLogSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("LogGroupName" Data..= logGroupName)
          ]
      )

instance Data.ToPath CreateLogSubscription where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLogSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLogSubscriptionResponse' smart constructor.
data CreateLogSubscriptionResponse = CreateLogSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLogSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLogSubscriptionResponse_httpStatus' - The response's http status code.
newCreateLogSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLogSubscriptionResponse
newCreateLogSubscriptionResponse pHttpStatus_ =
  CreateLogSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createLogSubscriptionResponse_httpStatus :: Lens.Lens' CreateLogSubscriptionResponse Prelude.Int
createLogSubscriptionResponse_httpStatus = Lens.lens (\CreateLogSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateLogSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateLogSubscriptionResponse)

instance Prelude.NFData CreateLogSubscriptionResponse where
  rnf CreateLogSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
