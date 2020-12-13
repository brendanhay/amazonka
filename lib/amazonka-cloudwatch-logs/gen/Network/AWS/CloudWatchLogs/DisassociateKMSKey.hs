{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DisassociateKMSKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the associated AWS Key Management Service (AWS KMS) customer master key (CMK) from the specified log group.
--
-- After the AWS KMS CMK is disassociated from the log group, AWS CloudWatch Logs stops encrypting newly ingested data for the log group. All previously ingested data remains encrypted, and AWS CloudWatch Logs requires permissions for the CMK whenever the encrypted data is requested.
-- Note that it can take up to 5 minutes for this operation to take effect.
module Network.AWS.CloudWatchLogs.DisassociateKMSKey
  ( -- * Creating a request
    DisassociateKMSKey (..),
    mkDisassociateKMSKey,

    -- ** Request lenses
    dkkLogGroupName,

    -- * Destructuring the response
    DisassociateKMSKeyResponse (..),
    mkDisassociateKMSKeyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateKMSKey' smart constructor.
newtype DisassociateKMSKey = DisassociateKMSKey'
  { -- | The name of the log group.
    logGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateKMSKey' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
mkDisassociateKMSKey ::
  -- | 'logGroupName'
  Lude.Text ->
  DisassociateKMSKey
mkDisassociateKMSKey pLogGroupName_ =
  DisassociateKMSKey' {logGroupName = pLogGroupName_}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkkLogGroupName :: Lens.Lens' DisassociateKMSKey Lude.Text
dkkLogGroupName = Lens.lens (logGroupName :: DisassociateKMSKey -> Lude.Text) (\s a -> s {logGroupName = a} :: DisassociateKMSKey)
{-# DEPRECATED dkkLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.AWSRequest DisassociateKMSKey where
  type Rs DisassociateKMSKey = DisassociateKMSKeyResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull DisassociateKMSKeyResponse'

instance Lude.ToHeaders DisassociateKMSKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DisassociateKmsKey" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateKMSKey where
  toJSON DisassociateKMSKey' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("logGroupName" Lude..= logGroupName)])

instance Lude.ToPath DisassociateKMSKey where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateKMSKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateKMSKeyResponse' smart constructor.
data DisassociateKMSKeyResponse = DisassociateKMSKeyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateKMSKeyResponse' with the minimum fields required to make a request.
mkDisassociateKMSKeyResponse ::
  DisassociateKMSKeyResponse
mkDisassociateKMSKeyResponse = DisassociateKMSKeyResponse'
