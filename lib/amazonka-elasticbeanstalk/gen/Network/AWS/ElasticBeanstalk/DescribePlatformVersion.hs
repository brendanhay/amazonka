{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribePlatformVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a platform version. Provides full details. Compare to 'ListPlatformVersions' , which provides summary information about a list of platform versions.
--
-- For definitions of platform version and other platform-related terms, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary> .
module Network.AWS.ElasticBeanstalk.DescribePlatformVersion
  ( -- * Creating a request
    DescribePlatformVersion (..),
    mkDescribePlatformVersion,

    -- ** Request lenses
    dPlatformARN,

    -- * Destructuring the response
    DescribePlatformVersionResponse (..),
    mkDescribePlatformVersionResponse,

    -- ** Response lenses
    drsPlatformDescription,
    drsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePlatformVersion' smart constructor.
newtype DescribePlatformVersion = DescribePlatformVersion'
  { -- | The ARN of the platform version.
    platformARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePlatformVersion' with the minimum fields required to make a request.
--
-- * 'platformARN' - The ARN of the platform version.
mkDescribePlatformVersion ::
  DescribePlatformVersion
mkDescribePlatformVersion =
  DescribePlatformVersion' {platformARN = Lude.Nothing}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPlatformARN :: Lens.Lens' DescribePlatformVersion (Lude.Maybe Lude.Text)
dPlatformARN = Lens.lens (platformARN :: DescribePlatformVersion -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: DescribePlatformVersion)
{-# DEPRECATED dPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

instance Lude.AWSRequest DescribePlatformVersion where
  type Rs DescribePlatformVersion = DescribePlatformVersionResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribePlatformVersionResult"
      ( \s h x ->
          DescribePlatformVersionResponse'
            Lude.<$> (x Lude..@? "PlatformDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePlatformVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribePlatformVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePlatformVersion where
  toQuery DescribePlatformVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribePlatformVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "PlatformArn" Lude.=: platformARN
      ]

-- | /See:/ 'mkDescribePlatformVersionResponse' smart constructor.
data DescribePlatformVersionResponse = DescribePlatformVersionResponse'
  { -- | Detailed information about the platform version.
    platformDescription :: Lude.Maybe PlatformDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePlatformVersionResponse' with the minimum fields required to make a request.
--
-- * 'platformDescription' - Detailed information about the platform version.
-- * 'responseStatus' - The response status code.
mkDescribePlatformVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePlatformVersionResponse
mkDescribePlatformVersionResponse pResponseStatus_ =
  DescribePlatformVersionResponse'
    { platformDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Detailed information about the platform version.
--
-- /Note:/ Consider using 'platformDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPlatformDescription :: Lens.Lens' DescribePlatformVersionResponse (Lude.Maybe PlatformDescription)
drsPlatformDescription = Lens.lens (platformDescription :: DescribePlatformVersionResponse -> Lude.Maybe PlatformDescription) (\s a -> s {platformDescription = a} :: DescribePlatformVersionResponse)
{-# DEPRECATED drsPlatformDescription "Use generic-lens or generic-optics with 'platformDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribePlatformVersionResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribePlatformVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePlatformVersionResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
