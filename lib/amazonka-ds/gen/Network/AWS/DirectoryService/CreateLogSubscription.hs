{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateLogSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription to forward real-time Directory Service domain controller security logs to the specified Amazon CloudWatch log group in your AWS account.
module Network.AWS.DirectoryService.CreateLogSubscription
  ( -- * Creating a request
    CreateLogSubscription (..),
    mkCreateLogSubscription,

    -- ** Request lenses
    clsDirectoryId,
    clsLogGroupName,

    -- * Destructuring the response
    CreateLogSubscriptionResponse (..),
    mkCreateLogSubscriptionResponse,

    -- ** Response lenses
    clsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLogSubscription' smart constructor.
data CreateLogSubscription = CreateLogSubscription'
  { directoryId ::
      Lude.Text,
    logGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLogSubscription' with the minimum fields required to make a request.
--
-- * 'directoryId' - Identifier of the directory to which you want to subscribe and receive real-time logs to your specified CloudWatch log group.
-- * 'logGroupName' - The name of the CloudWatch log group where the real-time domain controller logs are forwarded.
mkCreateLogSubscription ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'logGroupName'
  Lude.Text ->
  CreateLogSubscription
mkCreateLogSubscription pDirectoryId_ pLogGroupName_ =
  CreateLogSubscription'
    { directoryId = pDirectoryId_,
      logGroupName = pLogGroupName_
    }

-- | Identifier of the directory to which you want to subscribe and receive real-time logs to your specified CloudWatch log group.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsDirectoryId :: Lens.Lens' CreateLogSubscription Lude.Text
clsDirectoryId = Lens.lens (directoryId :: CreateLogSubscription -> Lude.Text) (\s a -> s {directoryId = a} :: CreateLogSubscription)
{-# DEPRECATED clsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of the CloudWatch log group where the real-time domain controller logs are forwarded.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsLogGroupName :: Lens.Lens' CreateLogSubscription Lude.Text
clsLogGroupName = Lens.lens (logGroupName :: CreateLogSubscription -> Lude.Text) (\s a -> s {logGroupName = a} :: CreateLogSubscription)
{-# DEPRECATED clsLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.AWSRequest CreateLogSubscription where
  type Rs CreateLogSubscription = CreateLogSubscriptionResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateLogSubscriptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLogSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.CreateLogSubscription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLogSubscription where
  toJSON CreateLogSubscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("LogGroupName" Lude..= logGroupName)
          ]
      )

instance Lude.ToPath CreateLogSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLogSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLogSubscriptionResponse' smart constructor.
newtype CreateLogSubscriptionResponse = CreateLogSubscriptionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLogSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateLogSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLogSubscriptionResponse
mkCreateLogSubscriptionResponse pResponseStatus_ =
  CreateLogSubscriptionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsrsResponseStatus :: Lens.Lens' CreateLogSubscriptionResponse Lude.Int
clsrsResponseStatus = Lens.lens (responseStatus :: CreateLogSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLogSubscriptionResponse)
{-# DEPRECATED clsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
