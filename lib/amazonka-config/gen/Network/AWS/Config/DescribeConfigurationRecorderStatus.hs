{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigurationRecorderStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified configuration recorder. If a configuration recorder is not specified, this action returns the status of all configuration recorders associated with the account.
module Network.AWS.Config.DescribeConfigurationRecorderStatus
  ( -- * Creating a request
    DescribeConfigurationRecorderStatus (..),
    mkDescribeConfigurationRecorderStatus,

    -- ** Request lenses
    dcrsConfigurationRecorderNames,

    -- * Destructuring the response
    DescribeConfigurationRecorderStatusResponse (..),
    mkDescribeConfigurationRecorderStatusResponse,

    -- ** Response lenses
    dcrssrsConfigurationRecordersStatus,
    dcrssrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'DescribeConfigurationRecorderStatus' action.
--
-- /See:/ 'mkDescribeConfigurationRecorderStatus' smart constructor.
newtype DescribeConfigurationRecorderStatus = DescribeConfigurationRecorderStatus'
  { -- | The name(s) of the configuration recorder. If the name is not specified, the action returns the current status of all the configuration recorders associated with the account.
    configurationRecorderNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationRecorderStatus' with the minimum fields required to make a request.
--
-- * 'configurationRecorderNames' - The name(s) of the configuration recorder. If the name is not specified, the action returns the current status of all the configuration recorders associated with the account.
mkDescribeConfigurationRecorderStatus ::
  DescribeConfigurationRecorderStatus
mkDescribeConfigurationRecorderStatus =
  DescribeConfigurationRecorderStatus'
    { configurationRecorderNames =
        Lude.Nothing
    }

-- | The name(s) of the configuration recorder. If the name is not specified, the action returns the current status of all the configuration recorders associated with the account.
--
-- /Note:/ Consider using 'configurationRecorderNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsConfigurationRecorderNames :: Lens.Lens' DescribeConfigurationRecorderStatus (Lude.Maybe [Lude.Text])
dcrsConfigurationRecorderNames = Lens.lens (configurationRecorderNames :: DescribeConfigurationRecorderStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {configurationRecorderNames = a} :: DescribeConfigurationRecorderStatus)
{-# DEPRECATED dcrsConfigurationRecorderNames "Use generic-lens or generic-optics with 'configurationRecorderNames' instead." #-}

instance Lude.AWSRequest DescribeConfigurationRecorderStatus where
  type
    Rs DescribeConfigurationRecorderStatus =
      DescribeConfigurationRecorderStatusResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConfigurationRecorderStatusResponse'
            Lude.<$> (x Lude..?> "ConfigurationRecordersStatus" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigurationRecorderStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeConfigurationRecorderStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConfigurationRecorderStatus where
  toJSON DescribeConfigurationRecorderStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConfigurationRecorderNames" Lude..=)
              Lude.<$> configurationRecorderNames
          ]
      )

instance Lude.ToPath DescribeConfigurationRecorderStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigurationRecorderStatus where
  toQuery = Lude.const Lude.mempty

-- | The output for the 'DescribeConfigurationRecorderStatus' action, in JSON format.
--
-- /See:/ 'mkDescribeConfigurationRecorderStatusResponse' smart constructor.
data DescribeConfigurationRecorderStatusResponse = DescribeConfigurationRecorderStatusResponse'
  { -- | A list that contains status of the specified recorders.
    configurationRecordersStatus :: Lude.Maybe [ConfigurationRecorderStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationRecorderStatusResponse' with the minimum fields required to make a request.
--
-- * 'configurationRecordersStatus' - A list that contains status of the specified recorders.
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationRecorderStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationRecorderStatusResponse
mkDescribeConfigurationRecorderStatusResponse pResponseStatus_ =
  DescribeConfigurationRecorderStatusResponse'
    { configurationRecordersStatus =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list that contains status of the specified recorders.
--
-- /Note:/ Consider using 'configurationRecordersStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrssrsConfigurationRecordersStatus :: Lens.Lens' DescribeConfigurationRecorderStatusResponse (Lude.Maybe [ConfigurationRecorderStatus])
dcrssrsConfigurationRecordersStatus = Lens.lens (configurationRecordersStatus :: DescribeConfigurationRecorderStatusResponse -> Lude.Maybe [ConfigurationRecorderStatus]) (\s a -> s {configurationRecordersStatus = a} :: DescribeConfigurationRecorderStatusResponse)
{-# DEPRECATED dcrssrsConfigurationRecordersStatus "Use generic-lens or generic-optics with 'configurationRecordersStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrssrsResponseStatus :: Lens.Lens' DescribeConfigurationRecorderStatusResponse Lude.Int
dcrssrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationRecorderStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationRecorderStatusResponse)
{-# DEPRECATED dcrssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
