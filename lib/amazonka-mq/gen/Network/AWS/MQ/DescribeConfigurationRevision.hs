{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeConfigurationRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified configuration revision for the specified configuration.
module Network.AWS.MQ.DescribeConfigurationRevision
  ( -- * Creating a request
    DescribeConfigurationRevision (..),
    mkDescribeConfigurationRevision,

    -- ** Request lenses
    dcrConfigurationRevision,
    dcrConfigurationId,

    -- * Destructuring the response
    DescribeConfigurationRevisionResponse (..),
    mkDescribeConfigurationRevisionResponse,

    -- ** Response lenses
    dcrrsConfigurationId,
    dcrrsData,
    dcrrsCreated,
    dcrrsDescription,
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeConfigurationRevision' smart constructor.
data DescribeConfigurationRevision = DescribeConfigurationRevision'
  { configurationRevision ::
      Lude.Text,
    configurationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationRevision' with the minimum fields required to make a request.
--
-- * 'configurationId' - The unique ID that Amazon MQ generates for the configuration.
-- * 'configurationRevision' - The revision of the configuration.
mkDescribeConfigurationRevision ::
  -- | 'configurationRevision'
  Lude.Text ->
  -- | 'configurationId'
  Lude.Text ->
  DescribeConfigurationRevision
mkDescribeConfigurationRevision
  pConfigurationRevision_
  pConfigurationId_ =
    DescribeConfigurationRevision'
      { configurationRevision =
          pConfigurationRevision_,
        configurationId = pConfigurationId_
      }

-- | The revision of the configuration.
--
-- /Note:/ Consider using 'configurationRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigurationRevision :: Lens.Lens' DescribeConfigurationRevision Lude.Text
dcrConfigurationRevision = Lens.lens (configurationRevision :: DescribeConfigurationRevision -> Lude.Text) (\s a -> s {configurationRevision = a} :: DescribeConfigurationRevision)
{-# DEPRECATED dcrConfigurationRevision "Use generic-lens or generic-optics with 'configurationRevision' instead." #-}

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigurationId :: Lens.Lens' DescribeConfigurationRevision Lude.Text
dcrConfigurationId = Lens.lens (configurationId :: DescribeConfigurationRevision -> Lude.Text) (\s a -> s {configurationId = a} :: DescribeConfigurationRevision)
{-# DEPRECATED dcrConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

instance Lude.AWSRequest DescribeConfigurationRevision where
  type
    Rs DescribeConfigurationRevision =
      DescribeConfigurationRevisionResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConfigurationRevisionResponse'
            Lude.<$> (x Lude..?> "configurationId")
            Lude.<*> (x Lude..?> "data")
            Lude.<*> (x Lude..?> "created")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigurationRevision where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeConfigurationRevision where
  toPath DescribeConfigurationRevision' {..} =
    Lude.mconcat
      [ "/v1/configurations/",
        Lude.toBS configurationId,
        "/revisions/",
        Lude.toBS configurationRevision
      ]

instance Lude.ToQuery DescribeConfigurationRevision where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConfigurationRevisionResponse' smart constructor.
data DescribeConfigurationRevisionResponse = DescribeConfigurationRevisionResponse'
  { configurationId ::
      Lude.Maybe
        Lude.Text,
    data' ::
      Lude.Maybe
        Lude.Text,
    created ::
      Lude.Maybe
        Lude.Timestamp,
    description ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'DescribeConfigurationRevisionResponse' with the minimum fields required to make a request.
--
-- * 'configurationId' - Required. The unique ID that Amazon MQ generates for the configuration.
-- * 'created' - Required. The date and time of the configuration.
-- * 'data'' - Required. The base64-encoded XML configuration.
-- * 'description' - The description of the configuration.
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationRevisionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationRevisionResponse
mkDescribeConfigurationRevisionResponse pResponseStatus_ =
  DescribeConfigurationRevisionResponse'
    { configurationId =
        Lude.Nothing,
      data' = Lude.Nothing,
      created = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsConfigurationId :: Lens.Lens' DescribeConfigurationRevisionResponse (Lude.Maybe Lude.Text)
dcrrsConfigurationId = Lens.lens (configurationId :: DescribeConfigurationRevisionResponse -> Lude.Maybe Lude.Text) (\s a -> s {configurationId = a} :: DescribeConfigurationRevisionResponse)
{-# DEPRECATED dcrrsConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | Required. The base64-encoded XML configuration.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsData :: Lens.Lens' DescribeConfigurationRevisionResponse (Lude.Maybe Lude.Text)
dcrrsData = Lens.lens (data' :: DescribeConfigurationRevisionResponse -> Lude.Maybe Lude.Text) (\s a -> s {data' = a} :: DescribeConfigurationRevisionResponse)
{-# DEPRECATED dcrrsData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | Required. The date and time of the configuration.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCreated :: Lens.Lens' DescribeConfigurationRevisionResponse (Lude.Maybe Lude.Timestamp)
dcrrsCreated = Lens.lens (created :: DescribeConfigurationRevisionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: DescribeConfigurationRevisionResponse)
{-# DEPRECATED dcrrsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The description of the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsDescription :: Lens.Lens' DescribeConfigurationRevisionResponse (Lude.Maybe Lude.Text)
dcrrsDescription = Lens.lens (description :: DescribeConfigurationRevisionResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeConfigurationRevisionResponse)
{-# DEPRECATED dcrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeConfigurationRevisionResponse Lude.Int
dcrrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationRevisionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationRevisionResponse)
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
