{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutRemediationExceptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A remediation exception is when a specific resource is no longer considered for auto-remediation. This API adds a new exception or updates an exisiting exception for a specific resource with a specific AWS Config rule.
module Network.AWS.Config.PutRemediationExceptions
  ( -- * Creating a request
    PutRemediationExceptions (..),
    mkPutRemediationExceptions,

    -- ** Request lenses
    preConfigRuleName,
    preMessage,
    preExpirationTime,
    preResourceKeys,

    -- * Destructuring the response
    PutRemediationExceptionsResponse (..),
    mkPutRemediationExceptionsResponse,

    -- ** Response lenses
    prersFailedBatches,
    prersResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutRemediationExceptions' smart constructor.
data PutRemediationExceptions = PutRemediationExceptions'
  { -- | The name of the AWS Config rule for which you want to create remediation exception.
    configRuleName :: Lude.Text,
    -- | The message contains an explanation of the exception.
    message :: Lude.Maybe Lude.Text,
    -- | The exception is automatically deleted after the expiration date.
    expirationTime :: Lude.Maybe Lude.Timestamp,
    -- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
    resourceKeys :: Lude.NonEmpty RemediationExceptionResourceKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRemediationExceptions' with the minimum fields required to make a request.
--
-- * 'configRuleName' - The name of the AWS Config rule for which you want to create remediation exception.
-- * 'message' - The message contains an explanation of the exception.
-- * 'expirationTime' - The exception is automatically deleted after the expiration date.
-- * 'resourceKeys' - An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
mkPutRemediationExceptions ::
  -- | 'configRuleName'
  Lude.Text ->
  -- | 'resourceKeys'
  Lude.NonEmpty RemediationExceptionResourceKey ->
  PutRemediationExceptions
mkPutRemediationExceptions pConfigRuleName_ pResourceKeys_ =
  PutRemediationExceptions'
    { configRuleName = pConfigRuleName_,
      message = Lude.Nothing,
      expirationTime = Lude.Nothing,
      resourceKeys = pResourceKeys_
    }

-- | The name of the AWS Config rule for which you want to create remediation exception.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preConfigRuleName :: Lens.Lens' PutRemediationExceptions Lude.Text
preConfigRuleName = Lens.lens (configRuleName :: PutRemediationExceptions -> Lude.Text) (\s a -> s {configRuleName = a} :: PutRemediationExceptions)
{-# DEPRECATED preConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The message contains an explanation of the exception.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preMessage :: Lens.Lens' PutRemediationExceptions (Lude.Maybe Lude.Text)
preMessage = Lens.lens (message :: PutRemediationExceptions -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: PutRemediationExceptions)
{-# DEPRECATED preMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The exception is automatically deleted after the expiration date.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preExpirationTime :: Lens.Lens' PutRemediationExceptions (Lude.Maybe Lude.Timestamp)
preExpirationTime = Lens.lens (expirationTime :: PutRemediationExceptions -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationTime = a} :: PutRemediationExceptions)
{-# DEPRECATED preExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | An exception list of resource exception keys to be processed with the current request. AWS Config adds exception for each resource key. For example, AWS Config adds 3 exceptions for 3 resource keys.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preResourceKeys :: Lens.Lens' PutRemediationExceptions (Lude.NonEmpty RemediationExceptionResourceKey)
preResourceKeys = Lens.lens (resourceKeys :: PutRemediationExceptions -> Lude.NonEmpty RemediationExceptionResourceKey) (\s a -> s {resourceKeys = a} :: PutRemediationExceptions)
{-# DEPRECATED preResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

instance Lude.AWSRequest PutRemediationExceptions where
  type Rs PutRemediationExceptions = PutRemediationExceptionsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutRemediationExceptionsResponse'
            Lude.<$> (x Lude..?> "FailedBatches" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutRemediationExceptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.PutRemediationExceptions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRemediationExceptions where
  toJSON PutRemediationExceptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ConfigRuleName" Lude..= configRuleName),
            ("Message" Lude..=) Lude.<$> message,
            ("ExpirationTime" Lude..=) Lude.<$> expirationTime,
            Lude.Just ("ResourceKeys" Lude..= resourceKeys)
          ]
      )

instance Lude.ToPath PutRemediationExceptions where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRemediationExceptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutRemediationExceptionsResponse' smart constructor.
data PutRemediationExceptionsResponse = PutRemediationExceptionsResponse'
  { -- | Returns a list of failed remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
    failedBatches :: Lude.Maybe [FailedRemediationExceptionBatch],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRemediationExceptionsResponse' with the minimum fields required to make a request.
--
-- * 'failedBatches' - Returns a list of failed remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
-- * 'responseStatus' - The response status code.
mkPutRemediationExceptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutRemediationExceptionsResponse
mkPutRemediationExceptionsResponse pResponseStatus_ =
  PutRemediationExceptionsResponse'
    { failedBatches = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of failed remediation exceptions batch objects. Each object in the batch consists of a list of failed items and failure messages.
--
-- /Note:/ Consider using 'failedBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prersFailedBatches :: Lens.Lens' PutRemediationExceptionsResponse (Lude.Maybe [FailedRemediationExceptionBatch])
prersFailedBatches = Lens.lens (failedBatches :: PutRemediationExceptionsResponse -> Lude.Maybe [FailedRemediationExceptionBatch]) (\s a -> s {failedBatches = a} :: PutRemediationExceptionsResponse)
{-# DEPRECATED prersFailedBatches "Use generic-lens or generic-optics with 'failedBatches' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prersResponseStatus :: Lens.Lens' PutRemediationExceptionsResponse Lude.Int
prersResponseStatus = Lens.lens (responseStatus :: PutRemediationExceptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRemediationExceptionsResponse)
{-# DEPRECATED prersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
