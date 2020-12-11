{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details of a security configuration by returning the configuration JSON.
module Network.AWS.EMR.DescribeSecurityConfiguration
  ( -- * Creating a request
    DescribeSecurityConfiguration (..),
    mkDescribeSecurityConfiguration,

    -- ** Request lenses
    dName,

    -- * Destructuring the response
    DescribeSecurityConfigurationResponse (..),
    mkDescribeSecurityConfigurationResponse,

    -- ** Response lenses
    drsSecurityConfiguration,
    drsName,
    drsCreationDateTime,
    drsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSecurityConfiguration' smart constructor.
newtype DescribeSecurityConfiguration = DescribeSecurityConfiguration'
  { name ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSecurityConfiguration' with the minimum fields required to make a request.
--
-- * 'name' - The name of the security configuration.
mkDescribeSecurityConfiguration ::
  -- | 'name'
  Lude.Text ->
  DescribeSecurityConfiguration
mkDescribeSecurityConfiguration pName_ =
  DescribeSecurityConfiguration' {name = pName_}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DescribeSecurityConfiguration Lude.Text
dName = Lens.lens (name :: DescribeSecurityConfiguration -> Lude.Text) (\s a -> s {name = a} :: DescribeSecurityConfiguration)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribeSecurityConfiguration where
  type
    Rs DescribeSecurityConfiguration =
      DescribeSecurityConfigurationResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSecurityConfigurationResponse'
            Lude.<$> (x Lude..?> "SecurityConfiguration")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "CreationDateTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSecurityConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ElasticMapReduce.DescribeSecurityConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSecurityConfiguration where
  toJSON DescribeSecurityConfiguration' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DescribeSecurityConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSecurityConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSecurityConfigurationResponse' smart constructor.
data DescribeSecurityConfigurationResponse = DescribeSecurityConfigurationResponse'
  { securityConfiguration ::
      Lude.Maybe
        Lude.Text,
    name ::
      Lude.Maybe
        Lude.Text,
    creationDateTime ::
      Lude.Maybe
        Lude.Timestamp,
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

-- | Creates a value of 'DescribeSecurityConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'creationDateTime' - The date and time the security configuration was created
-- * 'name' - The name of the security configuration.
-- * 'responseStatus' - The response status code.
-- * 'securityConfiguration' - The security configuration details in JSON format.
mkDescribeSecurityConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSecurityConfigurationResponse
mkDescribeSecurityConfigurationResponse pResponseStatus_ =
  DescribeSecurityConfigurationResponse'
    { securityConfiguration =
        Lude.Nothing,
      name = Lude.Nothing,
      creationDateTime = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The security configuration details in JSON format.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsSecurityConfiguration :: Lens.Lens' DescribeSecurityConfigurationResponse (Lude.Maybe Lude.Text)
drsSecurityConfiguration = Lens.lens (securityConfiguration :: DescribeSecurityConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: DescribeSecurityConfigurationResponse)
{-# DEPRECATED drsSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsName :: Lens.Lens' DescribeSecurityConfigurationResponse (Lude.Maybe Lude.Text)
drsName = Lens.lens (name :: DescribeSecurityConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeSecurityConfigurationResponse)
{-# DEPRECATED drsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time the security configuration was created
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreationDateTime :: Lens.Lens' DescribeSecurityConfigurationResponse (Lude.Maybe Lude.Timestamp)
drsCreationDateTime = Lens.lens (creationDateTime :: DescribeSecurityConfigurationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: DescribeSecurityConfigurationResponse)
{-# DEPRECATED drsCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeSecurityConfigurationResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeSecurityConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSecurityConfigurationResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
