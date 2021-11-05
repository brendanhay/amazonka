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
-- Module      : Network.AWS.IoTThingsGraph.GetNamespaceDeletionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of a namespace deletion task.
module Network.AWS.IoTThingsGraph.GetNamespaceDeletionStatus
  ( -- * Creating a Request
    GetNamespaceDeletionStatus (..),
    newGetNamespaceDeletionStatus,

    -- * Destructuring the Response
    GetNamespaceDeletionStatusResponse (..),
    newGetNamespaceDeletionStatusResponse,

    -- * Response Lenses
    getNamespaceDeletionStatusResponse_status,
    getNamespaceDeletionStatusResponse_namespaceArn,
    getNamespaceDeletionStatusResponse_namespaceName,
    getNamespaceDeletionStatusResponse_errorCode,
    getNamespaceDeletionStatusResponse_errorMessage,
    getNamespaceDeletionStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTThingsGraph.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetNamespaceDeletionStatus' smart constructor.
data GetNamespaceDeletionStatus = GetNamespaceDeletionStatus'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNamespaceDeletionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetNamespaceDeletionStatus ::
  GetNamespaceDeletionStatus
newGetNamespaceDeletionStatus =
  GetNamespaceDeletionStatus'

instance Core.AWSRequest GetNamespaceDeletionStatus where
  type
    AWSResponse GetNamespaceDeletionStatus =
      GetNamespaceDeletionStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNamespaceDeletionStatusResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "namespaceArn")
            Prelude.<*> (x Core..?> "namespaceName")
            Prelude.<*> (x Core..?> "errorCode")
            Prelude.<*> (x Core..?> "errorMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNamespaceDeletionStatus

instance Prelude.NFData GetNamespaceDeletionStatus

instance Core.ToHeaders GetNamespaceDeletionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.GetNamespaceDeletionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetNamespaceDeletionStatus where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetNamespaceDeletionStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery GetNamespaceDeletionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNamespaceDeletionStatusResponse' smart constructor.
data GetNamespaceDeletionStatusResponse = GetNamespaceDeletionStatusResponse'
  { -- | The status of the deletion request.
    status :: Prelude.Maybe NamespaceDeletionStatus,
    -- | The ARN of the namespace that is being deleted.
    namespaceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the namespace that is being deleted.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | An error code returned by the namespace deletion task.
    errorCode :: Prelude.Maybe NamespaceDeletionStatusErrorCodes,
    -- | An error code returned by the namespace deletion task.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNamespaceDeletionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getNamespaceDeletionStatusResponse_status' - The status of the deletion request.
--
-- 'namespaceArn', 'getNamespaceDeletionStatusResponse_namespaceArn' - The ARN of the namespace that is being deleted.
--
-- 'namespaceName', 'getNamespaceDeletionStatusResponse_namespaceName' - The name of the namespace that is being deleted.
--
-- 'errorCode', 'getNamespaceDeletionStatusResponse_errorCode' - An error code returned by the namespace deletion task.
--
-- 'errorMessage', 'getNamespaceDeletionStatusResponse_errorMessage' - An error code returned by the namespace deletion task.
--
-- 'httpStatus', 'getNamespaceDeletionStatusResponse_httpStatus' - The response's http status code.
newGetNamespaceDeletionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNamespaceDeletionStatusResponse
newGetNamespaceDeletionStatusResponse pHttpStatus_ =
  GetNamespaceDeletionStatusResponse'
    { status =
        Prelude.Nothing,
      namespaceArn = Prelude.Nothing,
      namespaceName = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the deletion request.
getNamespaceDeletionStatusResponse_status :: Lens.Lens' GetNamespaceDeletionStatusResponse (Prelude.Maybe NamespaceDeletionStatus)
getNamespaceDeletionStatusResponse_status = Lens.lens (\GetNamespaceDeletionStatusResponse' {status} -> status) (\s@GetNamespaceDeletionStatusResponse' {} a -> s {status = a} :: GetNamespaceDeletionStatusResponse)

-- | The ARN of the namespace that is being deleted.
getNamespaceDeletionStatusResponse_namespaceArn :: Lens.Lens' GetNamespaceDeletionStatusResponse (Prelude.Maybe Prelude.Text)
getNamespaceDeletionStatusResponse_namespaceArn = Lens.lens (\GetNamespaceDeletionStatusResponse' {namespaceArn} -> namespaceArn) (\s@GetNamespaceDeletionStatusResponse' {} a -> s {namespaceArn = a} :: GetNamespaceDeletionStatusResponse)

-- | The name of the namespace that is being deleted.
getNamespaceDeletionStatusResponse_namespaceName :: Lens.Lens' GetNamespaceDeletionStatusResponse (Prelude.Maybe Prelude.Text)
getNamespaceDeletionStatusResponse_namespaceName = Lens.lens (\GetNamespaceDeletionStatusResponse' {namespaceName} -> namespaceName) (\s@GetNamespaceDeletionStatusResponse' {} a -> s {namespaceName = a} :: GetNamespaceDeletionStatusResponse)

-- | An error code returned by the namespace deletion task.
getNamespaceDeletionStatusResponse_errorCode :: Lens.Lens' GetNamespaceDeletionStatusResponse (Prelude.Maybe NamespaceDeletionStatusErrorCodes)
getNamespaceDeletionStatusResponse_errorCode = Lens.lens (\GetNamespaceDeletionStatusResponse' {errorCode} -> errorCode) (\s@GetNamespaceDeletionStatusResponse' {} a -> s {errorCode = a} :: GetNamespaceDeletionStatusResponse)

-- | An error code returned by the namespace deletion task.
getNamespaceDeletionStatusResponse_errorMessage :: Lens.Lens' GetNamespaceDeletionStatusResponse (Prelude.Maybe Prelude.Text)
getNamespaceDeletionStatusResponse_errorMessage = Lens.lens (\GetNamespaceDeletionStatusResponse' {errorMessage} -> errorMessage) (\s@GetNamespaceDeletionStatusResponse' {} a -> s {errorMessage = a} :: GetNamespaceDeletionStatusResponse)

-- | The response's http status code.
getNamespaceDeletionStatusResponse_httpStatus :: Lens.Lens' GetNamespaceDeletionStatusResponse Prelude.Int
getNamespaceDeletionStatusResponse_httpStatus = Lens.lens (\GetNamespaceDeletionStatusResponse' {httpStatus} -> httpStatus) (\s@GetNamespaceDeletionStatusResponse' {} a -> s {httpStatus = a} :: GetNamespaceDeletionStatusResponse)

instance
  Prelude.NFData
    GetNamespaceDeletionStatusResponse
