{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeRemediationConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more remediation configurations.
module Network.AWS.Config.DescribeRemediationConfigurations
  ( -- * Creating a request
    DescribeRemediationConfigurations (..),
    mkDescribeRemediationConfigurations,

    -- ** Request lenses
    drcConfigRuleNames,

    -- * Destructuring the response
    DescribeRemediationConfigurationsResponse (..),
    mkDescribeRemediationConfigurationsResponse,

    -- ** Response lenses
    drcsrsRemediationConfigurations,
    drcsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRemediationConfigurations' smart constructor.
newtype DescribeRemediationConfigurations = DescribeRemediationConfigurations'
  { -- | A list of AWS Config rule names of remediation configurations for which you want details.
    configRuleNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRemediationConfigurations' with the minimum fields required to make a request.
--
-- * 'configRuleNames' - A list of AWS Config rule names of remediation configurations for which you want details.
mkDescribeRemediationConfigurations ::
  DescribeRemediationConfigurations
mkDescribeRemediationConfigurations =
  DescribeRemediationConfigurations' {configRuleNames = Lude.mempty}

-- | A list of AWS Config rule names of remediation configurations for which you want details.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcConfigRuleNames :: Lens.Lens' DescribeRemediationConfigurations [Lude.Text]
drcConfigRuleNames = Lens.lens (configRuleNames :: DescribeRemediationConfigurations -> [Lude.Text]) (\s a -> s {configRuleNames = a} :: DescribeRemediationConfigurations)
{-# DEPRECATED drcConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

instance Lude.AWSRequest DescribeRemediationConfigurations where
  type
    Rs DescribeRemediationConfigurations =
      DescribeRemediationConfigurationsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRemediationConfigurationsResponse'
            Lude.<$> (x Lude..?> "RemediationConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRemediationConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeRemediationConfigurations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRemediationConfigurations where
  toJSON DescribeRemediationConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ConfigRuleNames" Lude..= configRuleNames)]
      )

instance Lude.ToPath DescribeRemediationConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRemediationConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRemediationConfigurationsResponse' smart constructor.
data DescribeRemediationConfigurationsResponse = DescribeRemediationConfigurationsResponse'
  { -- | Returns a remediation configuration object.
    remediationConfigurations :: Lude.Maybe [RemediationConfiguration],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRemediationConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'remediationConfigurations' - Returns a remediation configuration object.
-- * 'responseStatus' - The response status code.
mkDescribeRemediationConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRemediationConfigurationsResponse
mkDescribeRemediationConfigurationsResponse pResponseStatus_ =
  DescribeRemediationConfigurationsResponse'
    { remediationConfigurations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a remediation configuration object.
--
-- /Note:/ Consider using 'remediationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcsrsRemediationConfigurations :: Lens.Lens' DescribeRemediationConfigurationsResponse (Lude.Maybe [RemediationConfiguration])
drcsrsRemediationConfigurations = Lens.lens (remediationConfigurations :: DescribeRemediationConfigurationsResponse -> Lude.Maybe [RemediationConfiguration]) (\s a -> s {remediationConfigurations = a} :: DescribeRemediationConfigurationsResponse)
{-# DEPRECATED drcsrsRemediationConfigurations "Use generic-lens or generic-optics with 'remediationConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcsrsResponseStatus :: Lens.Lens' DescribeRemediationConfigurationsResponse Lude.Int
drcsrsResponseStatus = Lens.lens (responseStatus :: DescribeRemediationConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRemediationConfigurationsResponse)
{-# DEPRECATED drcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
