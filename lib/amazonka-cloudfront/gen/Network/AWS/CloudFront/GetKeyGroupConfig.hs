{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetKeyGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key group configuration.
--
-- To get a key group configuration, you must provide the key group’s identifier. If the key group is referenced in a distribution’s cache behavior, you can get the key group’s identifier using @ListDistributions@ or @GetDistribution@ . If the key group is not referenced in a cache behavior, you can get the identifier using @ListKeyGroups@ .
module Network.AWS.CloudFront.GetKeyGroupConfig
  ( -- * Creating a request
    GetKeyGroupConfig (..),
    mkGetKeyGroupConfig,

    -- ** Request lenses
    gkgcId,

    -- * Destructuring the response
    GetKeyGroupConfigResponse (..),
    mkGetKeyGroupConfigResponse,

    -- ** Response lenses
    gkgcrsETag,
    gkgcrsKeyGroupConfig,
    gkgcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetKeyGroupConfig' smart constructor.
newtype GetKeyGroupConfig = GetKeyGroupConfig' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetKeyGroupConfig' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the key group whose configuration you are getting. To get the identifier, use @ListKeyGroups@ .
mkGetKeyGroupConfig ::
  -- | 'id'
  Lude.Text ->
  GetKeyGroupConfig
mkGetKeyGroupConfig pId_ = GetKeyGroupConfig' {id = pId_}

-- | The identifier of the key group whose configuration you are getting. To get the identifier, use @ListKeyGroups@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgcId :: Lens.Lens' GetKeyGroupConfig Lude.Text
gkgcId = Lens.lens (id :: GetKeyGroupConfig -> Lude.Text) (\s a -> s {id = a} :: GetKeyGroupConfig)
{-# DEPRECATED gkgcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetKeyGroupConfig where
  type Rs GetKeyGroupConfig = GetKeyGroupConfigResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetKeyGroupConfigResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetKeyGroupConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetKeyGroupConfig where
  toPath GetKeyGroupConfig' {..} =
    Lude.mconcat ["/2020-05-31/key-group/", Lude.toBS id, "/config"]

instance Lude.ToQuery GetKeyGroupConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetKeyGroupConfigResponse' smart constructor.
data GetKeyGroupConfigResponse = GetKeyGroupConfigResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    keyGroupConfig ::
      Lude.Maybe KeyGroupConfig,
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

-- | Creates a value of 'GetKeyGroupConfigResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The identifier for this version of the key group.
-- * 'keyGroupConfig' - The key group configuration.
-- * 'responseStatus' - The response status code.
mkGetKeyGroupConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetKeyGroupConfigResponse
mkGetKeyGroupConfigResponse pResponseStatus_ =
  GetKeyGroupConfigResponse'
    { eTag = Lude.Nothing,
      keyGroupConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for this version of the key group.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgcrsETag :: Lens.Lens' GetKeyGroupConfigResponse (Lude.Maybe Lude.Text)
gkgcrsETag = Lens.lens (eTag :: GetKeyGroupConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetKeyGroupConfigResponse)
{-# DEPRECATED gkgcrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The key group configuration.
--
-- /Note:/ Consider using 'keyGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgcrsKeyGroupConfig :: Lens.Lens' GetKeyGroupConfigResponse (Lude.Maybe KeyGroupConfig)
gkgcrsKeyGroupConfig = Lens.lens (keyGroupConfig :: GetKeyGroupConfigResponse -> Lude.Maybe KeyGroupConfig) (\s a -> s {keyGroupConfig = a} :: GetKeyGroupConfigResponse)
{-# DEPRECATED gkgcrsKeyGroupConfig "Use generic-lens or generic-optics with 'keyGroupConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgcrsResponseStatus :: Lens.Lens' GetKeyGroupConfigResponse Lude.Int
gkgcrsResponseStatus = Lens.lens (responseStatus :: GetKeyGroupConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetKeyGroupConfigResponse)
{-# DEPRECATED gkgcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
