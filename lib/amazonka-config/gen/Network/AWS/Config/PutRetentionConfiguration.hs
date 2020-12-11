{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutRetentionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and updates the retention configuration with details about retention period (number of days) that AWS Config stores your historical information. The API creates the @RetentionConfiguration@ object and names the object as __default__ . When you have a @RetentionConfiguration@ object named __default__ , calling the API modifies the default object.
module Network.AWS.Config.PutRetentionConfiguration
  ( -- * Creating a request
    PutRetentionConfiguration (..),
    mkPutRetentionConfiguration,

    -- ** Request lenses
    prcRetentionPeriodInDays,

    -- * Destructuring the response
    PutRetentionConfigurationResponse (..),
    mkPutRetentionConfigurationResponse,

    -- ** Response lenses
    prsRetentionConfiguration,
    prsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutRetentionConfiguration' smart constructor.
newtype PutRetentionConfiguration = PutRetentionConfiguration'
  { retentionPeriodInDays ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRetentionConfiguration' with the minimum fields required to make a request.
--
-- * 'retentionPeriodInDays' - Number of days AWS Config stores your historical information.
mkPutRetentionConfiguration ::
  -- | 'retentionPeriodInDays'
  Lude.Natural ->
  PutRetentionConfiguration
mkPutRetentionConfiguration pRetentionPeriodInDays_ =
  PutRetentionConfiguration'
    { retentionPeriodInDays =
        pRetentionPeriodInDays_
    }

-- | Number of days AWS Config stores your historical information.
--
-- /Note:/ Consider using 'retentionPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcRetentionPeriodInDays :: Lens.Lens' PutRetentionConfiguration Lude.Natural
prcRetentionPeriodInDays = Lens.lens (retentionPeriodInDays :: PutRetentionConfiguration -> Lude.Natural) (\s a -> s {retentionPeriodInDays = a} :: PutRetentionConfiguration)
{-# DEPRECATED prcRetentionPeriodInDays "Use generic-lens or generic-optics with 'retentionPeriodInDays' instead." #-}

instance Lude.AWSRequest PutRetentionConfiguration where
  type
    Rs PutRetentionConfiguration =
      PutRetentionConfigurationResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutRetentionConfigurationResponse'
            Lude.<$> (x Lude..?> "RetentionConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutRetentionConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.PutRetentionConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRetentionConfiguration where
  toJSON PutRetentionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("RetentionPeriodInDays" Lude..= retentionPeriodInDays)
          ]
      )

instance Lude.ToPath PutRetentionConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRetentionConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutRetentionConfigurationResponse' smart constructor.
data PutRetentionConfigurationResponse = PutRetentionConfigurationResponse'
  { retentionConfiguration ::
      Lude.Maybe
        RetentionConfiguration,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRetentionConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'retentionConfiguration' - Returns a retention configuration object.
mkPutRetentionConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutRetentionConfigurationResponse
mkPutRetentionConfigurationResponse pResponseStatus_ =
  PutRetentionConfigurationResponse'
    { retentionConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a retention configuration object.
--
-- /Note:/ Consider using 'retentionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsRetentionConfiguration :: Lens.Lens' PutRetentionConfigurationResponse (Lude.Maybe RetentionConfiguration)
prsRetentionConfiguration = Lens.lens (retentionConfiguration :: PutRetentionConfigurationResponse -> Lude.Maybe RetentionConfiguration) (\s a -> s {retentionConfiguration = a} :: PutRetentionConfigurationResponse)
{-# DEPRECATED prsRetentionConfiguration "Use generic-lens or generic-optics with 'retentionConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsResponseStatus :: Lens.Lens' PutRetentionConfigurationResponse Lude.Int
prsResponseStatus = Lens.lens (responseStatus :: PutRetentionConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRetentionConfigurationResponse)
{-# DEPRECATED prsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
