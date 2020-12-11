{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DescribeIdentityUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage information for an identity, including number of datasets and data usage.
--
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.
module Network.AWS.CognitoSync.DescribeIdentityUsage
  ( -- * Creating a request
    DescribeIdentityUsage (..),
    mkDescribeIdentityUsage,

    -- ** Request lenses
    diuIdentityPoolId,
    diuIdentityId,

    -- * Destructuring the response
    DescribeIdentityUsageResponse (..),
    mkDescribeIdentityUsageResponse,

    -- ** Response lenses
    diursIdentityUsage,
    diursResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request for information about the usage of an identity pool.
--
-- /See:/ 'mkDescribeIdentityUsage' smart constructor.
data DescribeIdentityUsage = DescribeIdentityUsage'
  { identityPoolId ::
      Lude.Text,
    identityId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdentityUsage' with the minimum fields required to make a request.
--
-- * 'identityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
mkDescribeIdentityUsage ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'identityId'
  Lude.Text ->
  DescribeIdentityUsage
mkDescribeIdentityUsage pIdentityPoolId_ pIdentityId_ =
  DescribeIdentityUsage'
    { identityPoolId = pIdentityPoolId_,
      identityId = pIdentityId_
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diuIdentityPoolId :: Lens.Lens' DescribeIdentityUsage Lude.Text
diuIdentityPoolId = Lens.lens (identityPoolId :: DescribeIdentityUsage -> Lude.Text) (\s a -> s {identityPoolId = a} :: DescribeIdentityUsage)
{-# DEPRECATED diuIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diuIdentityId :: Lens.Lens' DescribeIdentityUsage Lude.Text
diuIdentityId = Lens.lens (identityId :: DescribeIdentityUsage -> Lude.Text) (\s a -> s {identityId = a} :: DescribeIdentityUsage)
{-# DEPRECATED diuIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.AWSRequest DescribeIdentityUsage where
  type Rs DescribeIdentityUsage = DescribeIdentityUsageResponse
  request = Req.get cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeIdentityUsageResponse'
            Lude.<$> (x Lude..?> "IdentityUsage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeIdentityUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeIdentityUsage where
  toPath DescribeIdentityUsage' {..} =
    Lude.mconcat
      [ "/identitypools/",
        Lude.toBS identityPoolId,
        "/identities/",
        Lude.toBS identityId
      ]

instance Lude.ToQuery DescribeIdentityUsage where
  toQuery = Lude.const Lude.mempty

-- | The response to a successful DescribeIdentityUsage request.
--
-- /See:/ 'mkDescribeIdentityUsageResponse' smart constructor.
data DescribeIdentityUsageResponse = DescribeIdentityUsageResponse'
  { identityUsage ::
      Lude.Maybe IdentityUsage,
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

-- | Creates a value of 'DescribeIdentityUsageResponse' with the minimum fields required to make a request.
--
-- * 'identityUsage' - Usage information for the identity.
-- * 'responseStatus' - The response status code.
mkDescribeIdentityUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIdentityUsageResponse
mkDescribeIdentityUsageResponse pResponseStatus_ =
  DescribeIdentityUsageResponse'
    { identityUsage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Usage information for the identity.
--
-- /Note:/ Consider using 'identityUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diursIdentityUsage :: Lens.Lens' DescribeIdentityUsageResponse (Lude.Maybe IdentityUsage)
diursIdentityUsage = Lens.lens (identityUsage :: DescribeIdentityUsageResponse -> Lude.Maybe IdentityUsage) (\s a -> s {identityUsage = a} :: DescribeIdentityUsageResponse)
{-# DEPRECATED diursIdentityUsage "Use generic-lens or generic-optics with 'identityUsage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diursResponseStatus :: Lens.Lens' DescribeIdentityUsageResponse Lude.Int
diursResponseStatus = Lens.lens (responseStatus :: DescribeIdentityUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIdentityUsageResponse)
{-# DEPRECATED diursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
