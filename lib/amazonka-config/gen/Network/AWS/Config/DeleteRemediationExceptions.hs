{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteRemediationExceptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more remediation exceptions mentioned in the resource keys.
module Network.AWS.Config.DeleteRemediationExceptions
  ( -- * Creating a request
    DeleteRemediationExceptions (..),
    mkDeleteRemediationExceptions,

    -- ** Request lenses
    dConfigRuleName,
    dResourceKeys,

    -- * Destructuring the response
    DeleteRemediationExceptionsResponse (..),
    mkDeleteRemediationExceptionsResponse,

    -- ** Response lenses
    drefrsFailedBatches,
    drefrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRemediationExceptions' smart constructor.
data DeleteRemediationExceptions = DeleteRemediationExceptions'
  { -- | The name of the AWS Config rule for which you want to delete remediation exception configuration.
    configRuleName :: Lude.Text,
    -- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
    resourceKeys :: Lude.NonEmpty RemediationExceptionResourceKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRemediationExceptions' with the minimum fields required to make a request.
--
-- * 'configRuleName' - The name of the AWS Config rule for which you want to delete remediation exception configuration.
-- * 'resourceKeys' - An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
mkDeleteRemediationExceptions ::
  -- | 'configRuleName'
  Lude.Text ->
  -- | 'resourceKeys'
  Lude.NonEmpty RemediationExceptionResourceKey ->
  DeleteRemediationExceptions
mkDeleteRemediationExceptions pConfigRuleName_ pResourceKeys_ =
  DeleteRemediationExceptions'
    { configRuleName = pConfigRuleName_,
      resourceKeys = pResourceKeys_
    }

-- | The name of the AWS Config rule for which you want to delete remediation exception configuration.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConfigRuleName :: Lens.Lens' DeleteRemediationExceptions Lude.Text
dConfigRuleName = Lens.lens (configRuleName :: DeleteRemediationExceptions -> Lude.Text) (\s a -> s {configRuleName = a} :: DeleteRemediationExceptions)
{-# DEPRECATED dConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceKeys :: Lens.Lens' DeleteRemediationExceptions (Lude.NonEmpty RemediationExceptionResourceKey)
dResourceKeys = Lens.lens (resourceKeys :: DeleteRemediationExceptions -> Lude.NonEmpty RemediationExceptionResourceKey) (\s a -> s {resourceKeys = a} :: DeleteRemediationExceptions)
{-# DEPRECATED dResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

instance Lude.AWSRequest DeleteRemediationExceptions where
  type
    Rs DeleteRemediationExceptions =
      DeleteRemediationExceptionsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRemediationExceptionsResponse'
            Lude.<$> (x Lude..?> "FailedBatches" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRemediationExceptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DeleteRemediationExceptions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRemediationExceptions where
  toJSON DeleteRemediationExceptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ConfigRuleName" Lude..= configRuleName),
            Lude.Just ("ResourceKeys" Lude..= resourceKeys)
          ]
      )

instance Lude.ToPath DeleteRemediationExceptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRemediationExceptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRemediationExceptionsResponse' smart constructor.
data DeleteRemediationExceptionsResponse = DeleteRemediationExceptionsResponse'
  { -- | Returns a list of failed delete remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
    failedBatches :: Lude.Maybe [FailedDeleteRemediationExceptionsBatch],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRemediationExceptionsResponse' with the minimum fields required to make a request.
--
-- * 'failedBatches' - Returns a list of failed delete remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
-- * 'responseStatus' - The response status code.
mkDeleteRemediationExceptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRemediationExceptionsResponse
mkDeleteRemediationExceptionsResponse pResponseStatus_ =
  DeleteRemediationExceptionsResponse'
    { failedBatches =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of failed delete remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
--
-- /Note:/ Consider using 'failedBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drefrsFailedBatches :: Lens.Lens' DeleteRemediationExceptionsResponse (Lude.Maybe [FailedDeleteRemediationExceptionsBatch])
drefrsFailedBatches = Lens.lens (failedBatches :: DeleteRemediationExceptionsResponse -> Lude.Maybe [FailedDeleteRemediationExceptionsBatch]) (\s a -> s {failedBatches = a} :: DeleteRemediationExceptionsResponse)
{-# DEPRECATED drefrsFailedBatches "Use generic-lens or generic-optics with 'failedBatches' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drefrsResponseStatus :: Lens.Lens' DeleteRemediationExceptionsResponse Lude.Int
drefrsResponseStatus = Lens.lens (responseStatus :: DeleteRemediationExceptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRemediationExceptionsResponse)
{-# DEPRECATED drefrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
