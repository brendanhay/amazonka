{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a key group.
--
-- When you update a key group, all the fields are updated with the values provided in the request. You cannot update some fields independent of others. To update a key group:
--
--     * Get the current key group with @GetKeyGroup@ or @GetKeyGroupConfig@ .
--
--
--     * Locally modify the fields in the key group that you want to update. For example, add or remove public key IDs.
--
--
--     * Call @UpdateKeyGroup@ with the entire key group object, including the fields that you modified and those that you didn’t.
module Network.AWS.CloudFront.UpdateKeyGroup
  ( -- * Creating a request
    UpdateKeyGroup (..),
    mkUpdateKeyGroup,

    -- ** Request lenses
    ukgIfMatch,
    ukgKeyGroupConfig,
    ukgId,

    -- * Destructuring the response
    UpdateKeyGroupResponse (..),
    mkUpdateKeyGroupResponse,

    -- ** Response lenses
    ukgrsETag,
    ukgrsKeyGroup,
    ukgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateKeyGroup' smart constructor.
data UpdateKeyGroup = UpdateKeyGroup'
  { ifMatch ::
      Lude.Maybe Lude.Text,
    keyGroupConfig :: KeyGroupConfig,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateKeyGroup' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the key group that you are updating.
-- * 'ifMatch' - The version of the key group that you are updating. The version is the key group’s @ETag@ value.
-- * 'keyGroupConfig' - The key group configuration.
mkUpdateKeyGroup ::
  -- | 'keyGroupConfig'
  KeyGroupConfig ->
  -- | 'id'
  Lude.Text ->
  UpdateKeyGroup
mkUpdateKeyGroup pKeyGroupConfig_ pId_ =
  UpdateKeyGroup'
    { ifMatch = Lude.Nothing,
      keyGroupConfig = pKeyGroupConfig_,
      id = pId_
    }

-- | The version of the key group that you are updating. The version is the key group’s @ETag@ value.
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgIfMatch :: Lens.Lens' UpdateKeyGroup (Lude.Maybe Lude.Text)
ukgIfMatch = Lens.lens (ifMatch :: UpdateKeyGroup -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: UpdateKeyGroup)
{-# DEPRECATED ukgIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The key group configuration.
--
-- /Note:/ Consider using 'keyGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgKeyGroupConfig :: Lens.Lens' UpdateKeyGroup KeyGroupConfig
ukgKeyGroupConfig = Lens.lens (keyGroupConfig :: UpdateKeyGroup -> KeyGroupConfig) (\s a -> s {keyGroupConfig = a} :: UpdateKeyGroup)
{-# DEPRECATED ukgKeyGroupConfig "Use generic-lens or generic-optics with 'keyGroupConfig' instead." #-}

-- | The identifier of the key group that you are updating.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgId :: Lens.Lens' UpdateKeyGroup Lude.Text
ukgId = Lens.lens (id :: UpdateKeyGroup -> Lude.Text) (\s a -> s {id = a} :: UpdateKeyGroup)
{-# DEPRECATED ukgId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateKeyGroup where
  type Rs UpdateKeyGroup = UpdateKeyGroupResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateKeyGroupResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateKeyGroup where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}KeyGroupConfig"
      Lude.. keyGroupConfig

instance Lude.ToHeaders UpdateKeyGroup where
  toHeaders UpdateKeyGroup' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath UpdateKeyGroup where
  toPath UpdateKeyGroup' {..} =
    Lude.mconcat ["/2020-05-31/key-group/", Lude.toBS id]

instance Lude.ToQuery UpdateKeyGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateKeyGroupResponse' smart constructor.
data UpdateKeyGroupResponse = UpdateKeyGroupResponse'
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

-- | Creates a value of 'UpdateKeyGroupResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The identifier for this version of the key group.
-- * 'keyGroup' - The key group that was just updated.
-- * 'responseStatus' - The response status code.
mkUpdateKeyGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateKeyGroupResponse
mkUpdateKeyGroupResponse pResponseStatus_ =
  UpdateKeyGroupResponse'
    { eTag = Lude.Nothing,
      keyGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for this version of the key group.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgrsETag :: Lens.Lens' UpdateKeyGroupResponse (Lude.Maybe Lude.Text)
ukgrsETag = Lens.lens (eTag :: UpdateKeyGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: UpdateKeyGroupResponse)
{-# DEPRECATED ukgrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The key group that was just updated.
--
-- /Note:/ Consider using 'keyGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgrsKeyGroup :: Lens.Lens' UpdateKeyGroupResponse (Lude.Maybe KeyGroup)
ukgrsKeyGroup = Lens.lens (keyGroup :: UpdateKeyGroupResponse -> Lude.Maybe KeyGroup) (\s a -> s {keyGroup = a} :: UpdateKeyGroupResponse)
{-# DEPRECATED ukgrsKeyGroup "Use generic-lens or generic-optics with 'keyGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukgrsResponseStatus :: Lens.Lens' UpdateKeyGroupResponse Lude.Int
ukgrsResponseStatus = Lens.lens (responseStatus :: UpdateKeyGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateKeyGroupResponse)
{-# DEPRECATED ukgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
