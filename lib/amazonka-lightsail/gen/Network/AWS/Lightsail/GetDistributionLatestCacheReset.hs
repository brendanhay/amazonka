{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDistributionLatestCacheReset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the timestamp and status of the last cache reset of a specific Amazon Lightsail content delivery network (CDN) distribution.
module Network.AWS.Lightsail.GetDistributionLatestCacheReset
  ( -- * Creating a request
    GetDistributionLatestCacheReset (..),
    mkGetDistributionLatestCacheReset,

    -- ** Request lenses
    gdlcrDistributionName,

    -- * Destructuring the response
    GetDistributionLatestCacheResetResponse (..),
    mkGetDistributionLatestCacheResetResponse,

    -- ** Response lenses
    gdlcrrsStatus,
    gdlcrrsCreateTime,
    gdlcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDistributionLatestCacheReset' smart constructor.
newtype GetDistributionLatestCacheReset = GetDistributionLatestCacheReset'
  { distributionName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDistributionLatestCacheReset' with the minimum fields required to make a request.
--
-- * 'distributionName' - The name of the distribution for which to return the timestamp of the last cache reset.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
-- When omitted, the response includes the latest cache reset timestamp of all your distributions.
mkGetDistributionLatestCacheReset ::
  GetDistributionLatestCacheReset
mkGetDistributionLatestCacheReset =
  GetDistributionLatestCacheReset' {distributionName = Lude.Nothing}

-- | The name of the distribution for which to return the timestamp of the last cache reset.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
-- When omitted, the response includes the latest cache reset timestamp of all your distributions.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlcrDistributionName :: Lens.Lens' GetDistributionLatestCacheReset (Lude.Maybe Lude.Text)
gdlcrDistributionName = Lens.lens (distributionName :: GetDistributionLatestCacheReset -> Lude.Maybe Lude.Text) (\s a -> s {distributionName = a} :: GetDistributionLatestCacheReset)
{-# DEPRECATED gdlcrDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

instance Lude.AWSRequest GetDistributionLatestCacheReset where
  type
    Rs GetDistributionLatestCacheReset =
      GetDistributionLatestCacheResetResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDistributionLatestCacheResetResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "createTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDistributionLatestCacheReset where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetDistributionLatestCacheReset" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDistributionLatestCacheReset where
  toJSON GetDistributionLatestCacheReset' {..} =
    Lude.object
      ( Lude.catMaybes
          [("distributionName" Lude..=) Lude.<$> distributionName]
      )

instance Lude.ToPath GetDistributionLatestCacheReset where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDistributionLatestCacheReset where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDistributionLatestCacheResetResponse' smart constructor.
data GetDistributionLatestCacheResetResponse = GetDistributionLatestCacheResetResponse'
  { status ::
      Lude.Maybe
        Lude.Text,
    createTime ::
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

-- | Creates a value of 'GetDistributionLatestCacheResetResponse' with the minimum fields required to make a request.
--
-- * 'createTime' - The timestamp of the last cache reset (e.g., @1479734909.17@ ) in Unix time format.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the last cache reset.
mkGetDistributionLatestCacheResetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDistributionLatestCacheResetResponse
mkGetDistributionLatestCacheResetResponse pResponseStatus_ =
  GetDistributionLatestCacheResetResponse'
    { status = Lude.Nothing,
      createTime = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the last cache reset.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlcrrsStatus :: Lens.Lens' GetDistributionLatestCacheResetResponse (Lude.Maybe Lude.Text)
gdlcrrsStatus = Lens.lens (status :: GetDistributionLatestCacheResetResponse -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: GetDistributionLatestCacheResetResponse)
{-# DEPRECATED gdlcrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The timestamp of the last cache reset (e.g., @1479734909.17@ ) in Unix time format.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlcrrsCreateTime :: Lens.Lens' GetDistributionLatestCacheResetResponse (Lude.Maybe Lude.Timestamp)
gdlcrrsCreateTime = Lens.lens (createTime :: GetDistributionLatestCacheResetResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: GetDistributionLatestCacheResetResponse)
{-# DEPRECATED gdlcrrsCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlcrrsResponseStatus :: Lens.Lens' GetDistributionLatestCacheResetResponse Lude.Int
gdlcrrsResponseStatus = Lens.lens (responseStatus :: GetDistributionLatestCacheResetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDistributionLatestCacheResetResponse)
{-# DEPRECATED gdlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
