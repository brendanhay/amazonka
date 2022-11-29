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
-- Module      : Amazonka.StorageGateway.NotifyWhenUploaded
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends you notification through CloudWatch Events when all files written
-- to your file share have been uploaded to S3. Amazon S3.
--
-- Storage Gateway can send a notification through Amazon CloudWatch Events
-- when all files written to your file share up to that point in time have
-- been uploaded to Amazon S3. These files include files written to the
-- file share up to the time that you make a request for notification. When
-- the upload is done, Storage Gateway sends you notification through an
-- Amazon CloudWatch Event. You can configure CloudWatch Events to send the
-- notification through event targets such as Amazon SNS or Lambda
-- function. This operation is only supported for S3 File Gateways.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/monitoring-file-gateway.html#get-upload-notification Getting file upload notification>
-- in the /Storage Gateway User Guide/.
module Amazonka.StorageGateway.NotifyWhenUploaded
  ( -- * Creating a Request
    NotifyWhenUploaded (..),
    newNotifyWhenUploaded,

    -- * Request Lenses
    notifyWhenUploaded_fileShareARN,

    -- * Destructuring the Response
    NotifyWhenUploadedResponse (..),
    newNotifyWhenUploadedResponse,

    -- * Response Lenses
    notifyWhenUploadedResponse_fileShareARN,
    notifyWhenUploadedResponse_notificationId,
    notifyWhenUploadedResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newNotifyWhenUploaded' smart constructor.
data NotifyWhenUploaded = NotifyWhenUploaded'
  { fileShareARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyWhenUploaded' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARN', 'notifyWhenUploaded_fileShareARN' - Undocumented member.
newNotifyWhenUploaded ::
  -- | 'fileShareARN'
  Prelude.Text ->
  NotifyWhenUploaded
newNotifyWhenUploaded pFileShareARN_ =
  NotifyWhenUploaded' {fileShareARN = pFileShareARN_}

-- | Undocumented member.
notifyWhenUploaded_fileShareARN :: Lens.Lens' NotifyWhenUploaded Prelude.Text
notifyWhenUploaded_fileShareARN = Lens.lens (\NotifyWhenUploaded' {fileShareARN} -> fileShareARN) (\s@NotifyWhenUploaded' {} a -> s {fileShareARN = a} :: NotifyWhenUploaded)

instance Core.AWSRequest NotifyWhenUploaded where
  type
    AWSResponse NotifyWhenUploaded =
      NotifyWhenUploadedResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          NotifyWhenUploadedResponse'
            Prelude.<$> (x Core..?> "FileShareARN")
            Prelude.<*> (x Core..?> "NotificationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable NotifyWhenUploaded where
  hashWithSalt _salt NotifyWhenUploaded' {..} =
    _salt `Prelude.hashWithSalt` fileShareARN

instance Prelude.NFData NotifyWhenUploaded where
  rnf NotifyWhenUploaded' {..} =
    Prelude.rnf fileShareARN

instance Core.ToHeaders NotifyWhenUploaded where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.NotifyWhenUploaded" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON NotifyWhenUploaded where
  toJSON NotifyWhenUploaded' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("FileShareARN" Core..= fileShareARN)]
      )

instance Core.ToPath NotifyWhenUploaded where
  toPath = Prelude.const "/"

instance Core.ToQuery NotifyWhenUploaded where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyWhenUploadedResponse' smart constructor.
data NotifyWhenUploadedResponse = NotifyWhenUploadedResponse'
  { fileShareARN :: Prelude.Maybe Prelude.Text,
    notificationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyWhenUploadedResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARN', 'notifyWhenUploadedResponse_fileShareARN' - Undocumented member.
--
-- 'notificationId', 'notifyWhenUploadedResponse_notificationId' - Undocumented member.
--
-- 'httpStatus', 'notifyWhenUploadedResponse_httpStatus' - The response's http status code.
newNotifyWhenUploadedResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  NotifyWhenUploadedResponse
newNotifyWhenUploadedResponse pHttpStatus_ =
  NotifyWhenUploadedResponse'
    { fileShareARN =
        Prelude.Nothing,
      notificationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
notifyWhenUploadedResponse_fileShareARN :: Lens.Lens' NotifyWhenUploadedResponse (Prelude.Maybe Prelude.Text)
notifyWhenUploadedResponse_fileShareARN = Lens.lens (\NotifyWhenUploadedResponse' {fileShareARN} -> fileShareARN) (\s@NotifyWhenUploadedResponse' {} a -> s {fileShareARN = a} :: NotifyWhenUploadedResponse)

-- | Undocumented member.
notifyWhenUploadedResponse_notificationId :: Lens.Lens' NotifyWhenUploadedResponse (Prelude.Maybe Prelude.Text)
notifyWhenUploadedResponse_notificationId = Lens.lens (\NotifyWhenUploadedResponse' {notificationId} -> notificationId) (\s@NotifyWhenUploadedResponse' {} a -> s {notificationId = a} :: NotifyWhenUploadedResponse)

-- | The response's http status code.
notifyWhenUploadedResponse_httpStatus :: Lens.Lens' NotifyWhenUploadedResponse Prelude.Int
notifyWhenUploadedResponse_httpStatus = Lens.lens (\NotifyWhenUploadedResponse' {httpStatus} -> httpStatus) (\s@NotifyWhenUploadedResponse' {} a -> s {httpStatus = a} :: NotifyWhenUploadedResponse)

instance Prelude.NFData NotifyWhenUploadedResponse where
  rnf NotifyWhenUploadedResponse' {..} =
    Prelude.rnf fileShareARN
      `Prelude.seq` Prelude.rnf notificationId
      `Prelude.seq` Prelude.rnf httpStatus
