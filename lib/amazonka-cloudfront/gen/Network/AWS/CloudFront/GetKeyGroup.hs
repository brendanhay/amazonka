{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key group, including the date and time when the key group was last modified.
--
-- To get a key group, you must provide the key group’s identifier. If the key group is referenced in a distribution’s cache behavior, you can get the key group’s identifier using @ListDistributions@ or @GetDistribution@ . If the key group is not referenced in a cache behavior, you can get the identifier using @ListKeyGroups@ .
module Network.AWS.CloudFront.GetKeyGroup
  ( -- * Creating a request
    GetKeyGroup (..),
    mkGetKeyGroup,

    -- ** Request lenses
    gkgId,

    -- * Destructuring the response
    GetKeyGroupResponse (..),
    mkGetKeyGroupResponse,

    -- ** Response lenses
    gkgrsETag,
    gkgrsKeyGroup,
    gkgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetKeyGroup' smart constructor.
newtype GetKeyGroup = GetKeyGroup' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetKeyGroup' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the key group that you are getting. To get the identifier, use @ListKeyGroups@ .
mkGetKeyGroup ::
  -- | 'id'
  Lude.Text ->
  GetKeyGroup
mkGetKeyGroup pId_ = GetKeyGroup' {id = pId_}

-- | The identifier of the key group that you are getting. To get the identifier, use @ListKeyGroups@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgId :: Lens.Lens' GetKeyGroup Lude.Text
gkgId = Lens.lens (id :: GetKeyGroup -> Lude.Text) (\s a -> s {id = a} :: GetKeyGroup)
{-# DEPRECATED gkgId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetKeyGroup where
  type Rs GetKeyGroup = GetKeyGroupResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetKeyGroupResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetKeyGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetKeyGroup where
  toPath GetKeyGroup' {..} =
    Lude.mconcat ["/2020-05-31/key-group/", Lude.toBS id]

instance Lude.ToQuery GetKeyGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetKeyGroupResponse' smart constructor.
data GetKeyGroupResponse = GetKeyGroupResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    keyGroup :: Lude.Maybe KeyGroup,
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

-- | Creates a value of 'GetKeyGroupResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The identifier for this version of the key group.
-- * 'keyGroup' - The key group.
-- * 'responseStatus' - The response status code.
mkGetKeyGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetKeyGroupResponse
mkGetKeyGroupResponse pResponseStatus_ =
  GetKeyGroupResponse'
    { eTag = Lude.Nothing,
      keyGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for this version of the key group.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgrsETag :: Lens.Lens' GetKeyGroupResponse (Lude.Maybe Lude.Text)
gkgrsETag = Lens.lens (eTag :: GetKeyGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetKeyGroupResponse)
{-# DEPRECATED gkgrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The key group.
--
-- /Note:/ Consider using 'keyGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgrsKeyGroup :: Lens.Lens' GetKeyGroupResponse (Lude.Maybe KeyGroup)
gkgrsKeyGroup = Lens.lens (keyGroup :: GetKeyGroupResponse -> Lude.Maybe KeyGroup) (\s a -> s {keyGroup = a} :: GetKeyGroupResponse)
{-# DEPRECATED gkgrsKeyGroup "Use generic-lens or generic-optics with 'keyGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkgrsResponseStatus :: Lens.Lens' GetKeyGroupResponse Lude.Int
gkgrsResponseStatus = Lens.lens (responseStatus :: GetKeyGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetKeyGroupResponse)
{-# DEPRECATED gkgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
