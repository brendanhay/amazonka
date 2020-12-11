{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes for a list of configuration item IDs.
module Network.AWS.Discovery.DescribeConfigurations
  ( -- * Creating a request
    DescribeConfigurations (..),
    mkDescribeConfigurations,

    -- ** Request lenses
    dcConfigurationIds,

    -- * Destructuring the response
    DescribeConfigurationsResponse (..),
    mkDescribeConfigurationsResponse,

    -- ** Response lenses
    dcrsConfigurations,
    dcrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeConfigurations' smart constructor.
newtype DescribeConfigurations = DescribeConfigurations'
  { configurationIds ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurations' with the minimum fields required to make a request.
--
-- * 'configurationIds' - One or more configuration IDs.
mkDescribeConfigurations ::
  DescribeConfigurations
mkDescribeConfigurations =
  DescribeConfigurations' {configurationIds = Lude.mempty}

-- | One or more configuration IDs.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConfigurationIds :: Lens.Lens' DescribeConfigurations [Lude.Text]
dcConfigurationIds = Lens.lens (configurationIds :: DescribeConfigurations -> [Lude.Text]) (\s a -> s {configurationIds = a} :: DescribeConfigurations)
{-# DEPRECATED dcConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

instance Lude.AWSRequest DescribeConfigurations where
  type Rs DescribeConfigurations = DescribeConfigurationsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConfigurationsResponse'
            Lude.<$> (x Lude..?> "configurations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.DescribeConfigurations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConfigurations where
  toJSON DescribeConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("configurationIds" Lude..= configurationIds)]
      )

instance Lude.ToPath DescribeConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConfigurationsResponse' smart constructor.
data DescribeConfigurationsResponse = DescribeConfigurationsResponse'
  { configurations ::
      Lude.Maybe
        [ Lude.HashMap
            Lude.Text
            (Lude.Text)
        ],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'configurations' - A key in the response map. The value is an array of data.
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationsResponse
mkDescribeConfigurationsResponse pResponseStatus_ =
  DescribeConfigurationsResponse'
    { configurations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A key in the response map. The value is an array of data.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsConfigurations :: Lens.Lens' DescribeConfigurationsResponse (Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)])
dcrsConfigurations = Lens.lens (configurations :: DescribeConfigurationsResponse -> Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {configurations = a} :: DescribeConfigurationsResponse)
{-# DEPRECATED dcrsConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeConfigurationsResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationsResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
