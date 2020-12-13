{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigurationRecorders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details for the specified configuration recorders. If the configuration recorder is not specified, this action returns the details for all configuration recorders associated with the account.
module Network.AWS.Config.DescribeConfigurationRecorders
  ( -- * Creating a request
    DescribeConfigurationRecorders (..),
    mkDescribeConfigurationRecorders,

    -- ** Request lenses
    dcrConfigurationRecorderNames,

    -- * Destructuring the response
    DescribeConfigurationRecordersResponse (..),
    mkDescribeConfigurationRecordersResponse,

    -- ** Response lenses
    dcrsrsConfigurationRecorders,
    dcrsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'DescribeConfigurationRecorders' action.
--
-- /See:/ 'mkDescribeConfigurationRecorders' smart constructor.
newtype DescribeConfigurationRecorders = DescribeConfigurationRecorders'
  { -- | A list of configuration recorder names.
    configurationRecorderNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationRecorders' with the minimum fields required to make a request.
--
-- * 'configurationRecorderNames' - A list of configuration recorder names.
mkDescribeConfigurationRecorders ::
  DescribeConfigurationRecorders
mkDescribeConfigurationRecorders =
  DescribeConfigurationRecorders'
    { configurationRecorderNames =
        Lude.Nothing
    }

-- | A list of configuration recorder names.
--
-- /Note:/ Consider using 'configurationRecorderNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigurationRecorderNames :: Lens.Lens' DescribeConfigurationRecorders (Lude.Maybe [Lude.Text])
dcrConfigurationRecorderNames = Lens.lens (configurationRecorderNames :: DescribeConfigurationRecorders -> Lude.Maybe [Lude.Text]) (\s a -> s {configurationRecorderNames = a} :: DescribeConfigurationRecorders)
{-# DEPRECATED dcrConfigurationRecorderNames "Use generic-lens or generic-optics with 'configurationRecorderNames' instead." #-}

instance Lude.AWSRequest DescribeConfigurationRecorders where
  type
    Rs DescribeConfigurationRecorders =
      DescribeConfigurationRecordersResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConfigurationRecordersResponse'
            Lude.<$> (x Lude..?> "ConfigurationRecorders" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigurationRecorders where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeConfigurationRecorders" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConfigurationRecorders where
  toJSON DescribeConfigurationRecorders' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConfigurationRecorderNames" Lude..=)
              Lude.<$> configurationRecorderNames
          ]
      )

instance Lude.ToPath DescribeConfigurationRecorders where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigurationRecorders where
  toQuery = Lude.const Lude.mempty

-- | The output for the 'DescribeConfigurationRecorders' action.
--
-- /See:/ 'mkDescribeConfigurationRecordersResponse' smart constructor.
data DescribeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse'
  { -- | A list that contains the descriptions of the specified configuration recorders.
    configurationRecorders :: Lude.Maybe [ConfigurationRecorder],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationRecordersResponse' with the minimum fields required to make a request.
--
-- * 'configurationRecorders' - A list that contains the descriptions of the specified configuration recorders.
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationRecordersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationRecordersResponse
mkDescribeConfigurationRecordersResponse pResponseStatus_ =
  DescribeConfigurationRecordersResponse'
    { configurationRecorders =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list that contains the descriptions of the specified configuration recorders.
--
-- /Note:/ Consider using 'configurationRecorders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsrsConfigurationRecorders :: Lens.Lens' DescribeConfigurationRecordersResponse (Lude.Maybe [ConfigurationRecorder])
dcrsrsConfigurationRecorders = Lens.lens (configurationRecorders :: DescribeConfigurationRecordersResponse -> Lude.Maybe [ConfigurationRecorder]) (\s a -> s {configurationRecorders = a} :: DescribeConfigurationRecordersResponse)
{-# DEPRECATED dcrsrsConfigurationRecorders "Use generic-lens or generic-optics with 'configurationRecorders' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsrsResponseStatus :: Lens.Lens' DescribeConfigurationRecordersResponse Lude.Int
dcrsrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationRecordersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationRecordersResponse)
{-# DEPRECATED dcrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
