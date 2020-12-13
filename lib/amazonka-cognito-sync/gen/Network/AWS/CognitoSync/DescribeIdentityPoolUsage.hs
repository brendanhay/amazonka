{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DescribeIdentityPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage details (for example, data storage) about a particular identity pool.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.DescribeIdentityPoolUsage
  ( -- * Creating a request
    DescribeIdentityPoolUsage (..),
    mkDescribeIdentityPoolUsage,

    -- ** Request lenses
    dipuIdentityPoolId,

    -- * Destructuring the response
    DescribeIdentityPoolUsageResponse (..),
    mkDescribeIdentityPoolUsageResponse,

    -- ** Response lenses
    dipursIdentityPoolUsage,
    dipursResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request for usage information about the identity pool.
--
-- /See:/ 'mkDescribeIdentityPoolUsage' smart constructor.
newtype DescribeIdentityPoolUsage = DescribeIdentityPoolUsage'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdentityPoolUsage' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
mkDescribeIdentityPoolUsage ::
  -- | 'identityPoolId'
  Lude.Text ->
  DescribeIdentityPoolUsage
mkDescribeIdentityPoolUsage pIdentityPoolId_ =
  DescribeIdentityPoolUsage' {identityPoolId = pIdentityPoolId_}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipuIdentityPoolId :: Lens.Lens' DescribeIdentityPoolUsage Lude.Text
dipuIdentityPoolId = Lens.lens (identityPoolId :: DescribeIdentityPoolUsage -> Lude.Text) (\s a -> s {identityPoolId = a} :: DescribeIdentityPoolUsage)
{-# DEPRECATED dipuIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Lude.AWSRequest DescribeIdentityPoolUsage where
  type
    Rs DescribeIdentityPoolUsage =
      DescribeIdentityPoolUsageResponse
  request = Req.get cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeIdentityPoolUsageResponse'
            Lude.<$> (x Lude..?> "IdentityPoolUsage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeIdentityPoolUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeIdentityPoolUsage where
  toPath DescribeIdentityPoolUsage' {..} =
    Lude.mconcat ["/identitypools/", Lude.toBS identityPoolId]

instance Lude.ToQuery DescribeIdentityPoolUsage where
  toQuery = Lude.const Lude.mempty

-- | Response to a successful DescribeIdentityPoolUsage request.
--
-- /See:/ 'mkDescribeIdentityPoolUsageResponse' smart constructor.
data DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse'
  { -- | Information about the usage of the identity pool.
    identityPoolUsage :: Lude.Maybe IdentityPoolUsage,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdentityPoolUsageResponse' with the minimum fields required to make a request.
--
-- * 'identityPoolUsage' - Information about the usage of the identity pool.
-- * 'responseStatus' - The response status code.
mkDescribeIdentityPoolUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIdentityPoolUsageResponse
mkDescribeIdentityPoolUsageResponse pResponseStatus_ =
  DescribeIdentityPoolUsageResponse'
    { identityPoolUsage =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the usage of the identity pool.
--
-- /Note:/ Consider using 'identityPoolUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipursIdentityPoolUsage :: Lens.Lens' DescribeIdentityPoolUsageResponse (Lude.Maybe IdentityPoolUsage)
dipursIdentityPoolUsage = Lens.lens (identityPoolUsage :: DescribeIdentityPoolUsageResponse -> Lude.Maybe IdentityPoolUsage) (\s a -> s {identityPoolUsage = a} :: DescribeIdentityPoolUsageResponse)
{-# DEPRECATED dipursIdentityPoolUsage "Use generic-lens or generic-optics with 'identityPoolUsage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipursResponseStatus :: Lens.Lens' DescribeIdentityPoolUsageResponse Lude.Int
dipursResponseStatus = Lens.lens (responseStatus :: DescribeIdentityPoolUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIdentityPoolUsageResponse)
{-# DEPRECATED dipursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
