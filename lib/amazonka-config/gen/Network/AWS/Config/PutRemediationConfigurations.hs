{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutRemediationConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the remediation configuration with a specific AWS Config rule with the selected target or action. The API creates the @RemediationConfiguration@ object for the AWS Config rule. The AWS Config rule must already exist for you to add a remediation configuration. The target (SSM document) must exist and have permissions to use the target.
module Network.AWS.Config.PutRemediationConfigurations
  ( -- * Creating a request
    PutRemediationConfigurations (..),
    mkPutRemediationConfigurations,

    -- ** Request lenses
    prcRemediationConfigurations,

    -- * Destructuring the response
    PutRemediationConfigurationsResponse (..),
    mkPutRemediationConfigurationsResponse,

    -- ** Response lenses
    prcrsFailedBatches,
    prcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutRemediationConfigurations' smart constructor.
newtype PutRemediationConfigurations = PutRemediationConfigurations'
  { -- | A list of remediation configuration objects.
    remediationConfigurations :: [RemediationConfiguration]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRemediationConfigurations' with the minimum fields required to make a request.
--
-- * 'remediationConfigurations' - A list of remediation configuration objects.
mkPutRemediationConfigurations ::
  PutRemediationConfigurations
mkPutRemediationConfigurations =
  PutRemediationConfigurations'
    { remediationConfigurations =
        Lude.mempty
    }

-- | A list of remediation configuration objects.
--
-- /Note:/ Consider using 'remediationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcRemediationConfigurations :: Lens.Lens' PutRemediationConfigurations [RemediationConfiguration]
prcRemediationConfigurations = Lens.lens (remediationConfigurations :: PutRemediationConfigurations -> [RemediationConfiguration]) (\s a -> s {remediationConfigurations = a} :: PutRemediationConfigurations)
{-# DEPRECATED prcRemediationConfigurations "Use generic-lens or generic-optics with 'remediationConfigurations' instead." #-}

instance Lude.AWSRequest PutRemediationConfigurations where
  type
    Rs PutRemediationConfigurations =
      PutRemediationConfigurationsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutRemediationConfigurationsResponse'
            Lude.<$> (x Lude..?> "FailedBatches" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutRemediationConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.PutRemediationConfigurations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRemediationConfigurations where
  toJSON PutRemediationConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("RemediationConfigurations" Lude..= remediationConfigurations)
          ]
      )

instance Lude.ToPath PutRemediationConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRemediationConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutRemediationConfigurationsResponse' smart constructor.
data PutRemediationConfigurationsResponse = PutRemediationConfigurationsResponse'
  { -- | Returns a list of failed remediation batch objects.
    failedBatches :: Lude.Maybe [FailedRemediationBatch],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRemediationConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'failedBatches' - Returns a list of failed remediation batch objects.
-- * 'responseStatus' - The response status code.
mkPutRemediationConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutRemediationConfigurationsResponse
mkPutRemediationConfigurationsResponse pResponseStatus_ =
  PutRemediationConfigurationsResponse'
    { failedBatches =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of failed remediation batch objects.
--
-- /Note:/ Consider using 'failedBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcrsFailedBatches :: Lens.Lens' PutRemediationConfigurationsResponse (Lude.Maybe [FailedRemediationBatch])
prcrsFailedBatches = Lens.lens (failedBatches :: PutRemediationConfigurationsResponse -> Lude.Maybe [FailedRemediationBatch]) (\s a -> s {failedBatches = a} :: PutRemediationConfigurationsResponse)
{-# DEPRECATED prcrsFailedBatches "Use generic-lens or generic-optics with 'failedBatches' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcrsResponseStatus :: Lens.Lens' PutRemediationConfigurationsResponse Lude.Int
prcrsResponseStatus = Lens.lens (responseStatus :: PutRemediationConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRemediationConfigurationsResponse)
{-# DEPRECATED prcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
