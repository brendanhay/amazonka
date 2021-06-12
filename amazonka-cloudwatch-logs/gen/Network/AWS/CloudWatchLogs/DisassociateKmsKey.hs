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
-- Module      : Network.AWS.CloudWatchLogs.DisassociateKmsKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the associated AWS Key Management Service (AWS KMS)
-- customer master key (CMK) from the specified log group.
--
-- After the AWS KMS CMK is disassociated from the log group, AWS
-- CloudWatch Logs stops encrypting newly ingested data for the log group.
-- All previously ingested data remains encrypted, and AWS CloudWatch Logs
-- requires permissions for the CMK whenever the encrypted data is
-- requested.
--
-- Note that it can take up to 5 minutes for this operation to take effect.
module Network.AWS.CloudWatchLogs.DisassociateKmsKey
  ( -- * Creating a Request
    DisassociateKmsKey (..),
    newDisassociateKmsKey,

    -- * Request Lenses
    disassociateKmsKey_logGroupName,

    -- * Destructuring the Response
    DisassociateKmsKeyResponse (..),
    newDisassociateKmsKeyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateKmsKey' smart constructor.
data DisassociateKmsKey = DisassociateKmsKey'
  { -- | The name of the log group.
    logGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateKmsKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'disassociateKmsKey_logGroupName' - The name of the log group.
newDisassociateKmsKey ::
  -- | 'logGroupName'
  Core.Text ->
  DisassociateKmsKey
newDisassociateKmsKey pLogGroupName_ =
  DisassociateKmsKey' {logGroupName = pLogGroupName_}

-- | The name of the log group.
disassociateKmsKey_logGroupName :: Lens.Lens' DisassociateKmsKey Core.Text
disassociateKmsKey_logGroupName = Lens.lens (\DisassociateKmsKey' {logGroupName} -> logGroupName) (\s@DisassociateKmsKey' {} a -> s {logGroupName = a} :: DisassociateKmsKey)

instance Core.AWSRequest DisassociateKmsKey where
  type
    AWSResponse DisassociateKmsKey =
      DisassociateKmsKeyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DisassociateKmsKeyResponse'

instance Core.Hashable DisassociateKmsKey

instance Core.NFData DisassociateKmsKey

instance Core.ToHeaders DisassociateKmsKey where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DisassociateKmsKey" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateKmsKey where
  toJSON DisassociateKmsKey' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("logGroupName" Core..= logGroupName)]
      )

instance Core.ToPath DisassociateKmsKey where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateKmsKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateKmsKeyResponse' smart constructor.
data DisassociateKmsKeyResponse = DisassociateKmsKeyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateKmsKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateKmsKeyResponse ::
  DisassociateKmsKeyResponse
newDisassociateKmsKeyResponse =
  DisassociateKmsKeyResponse'

instance Core.NFData DisassociateKmsKeyResponse
