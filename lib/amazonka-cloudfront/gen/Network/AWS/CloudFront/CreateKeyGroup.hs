{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a key group that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
-- To create a key group, you must specify at least one public key for the key group. After you create a key group, you can reference it from one or more cache behaviors. When you reference a key group in a cache behavior, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateKeyGroup
  ( -- * Creating a request
    CreateKeyGroup (..),
    mkCreateKeyGroup,

    -- ** Request lenses
    ckgKeyGroupConfig,

    -- * Destructuring the response
    CreateKeyGroupResponse (..),
    mkCreateKeyGroupResponse,

    -- ** Response lenses
    ckgrsETag,
    ckgrsLocation,
    ckgrsKeyGroup,
    ckgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateKeyGroup' smart constructor.
newtype CreateKeyGroup = CreateKeyGroup'
  { keyGroupConfig ::
      KeyGroupConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateKeyGroup' with the minimum fields required to make a request.
--
-- * 'keyGroupConfig' - A key group configuration.
mkCreateKeyGroup ::
  -- | 'keyGroupConfig'
  KeyGroupConfig ->
  CreateKeyGroup
mkCreateKeyGroup pKeyGroupConfig_ =
  CreateKeyGroup' {keyGroupConfig = pKeyGroupConfig_}

-- | A key group configuration.
--
-- /Note:/ Consider using 'keyGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckgKeyGroupConfig :: Lens.Lens' CreateKeyGroup KeyGroupConfig
ckgKeyGroupConfig = Lens.lens (keyGroupConfig :: CreateKeyGroup -> KeyGroupConfig) (\s a -> s {keyGroupConfig = a} :: CreateKeyGroup)
{-# DEPRECATED ckgKeyGroupConfig "Use generic-lens or generic-optics with 'keyGroupConfig' instead." #-}

instance Lude.AWSRequest CreateKeyGroup where
  type Rs CreateKeyGroup = CreateKeyGroupResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateKeyGroupResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateKeyGroup where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}KeyGroupConfig"
      Lude.. keyGroupConfig

instance Lude.ToHeaders CreateKeyGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateKeyGroup where
  toPath = Lude.const "/2020-05-31/key-group"

instance Lude.ToQuery CreateKeyGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateKeyGroupResponse' smart constructor.
data CreateKeyGroupResponse = CreateKeyGroupResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    location :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateKeyGroupResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The identifier for this version of the key group.
-- * 'keyGroup' - The key group that was just created.
-- * 'location' - The URL of the key group.
-- * 'responseStatus' - The response status code.
mkCreateKeyGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateKeyGroupResponse
mkCreateKeyGroupResponse pResponseStatus_ =
  CreateKeyGroupResponse'
    { eTag = Lude.Nothing,
      location = Lude.Nothing,
      keyGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for this version of the key group.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckgrsETag :: Lens.Lens' CreateKeyGroupResponse (Lude.Maybe Lude.Text)
ckgrsETag = Lens.lens (eTag :: CreateKeyGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateKeyGroupResponse)
{-# DEPRECATED ckgrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The URL of the key group.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckgrsLocation :: Lens.Lens' CreateKeyGroupResponse (Lude.Maybe Lude.Text)
ckgrsLocation = Lens.lens (location :: CreateKeyGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateKeyGroupResponse)
{-# DEPRECATED ckgrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The key group that was just created.
--
-- /Note:/ Consider using 'keyGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckgrsKeyGroup :: Lens.Lens' CreateKeyGroupResponse (Lude.Maybe KeyGroup)
ckgrsKeyGroup = Lens.lens (keyGroup :: CreateKeyGroupResponse -> Lude.Maybe KeyGroup) (\s a -> s {keyGroup = a} :: CreateKeyGroupResponse)
{-# DEPRECATED ckgrsKeyGroup "Use generic-lens or generic-optics with 'keyGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckgrsResponseStatus :: Lens.Lens' CreateKeyGroupResponse Lude.Int
ckgrsResponseStatus = Lens.lens (responseStatus :: CreateKeyGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateKeyGroupResponse)
{-# DEPRECATED ckgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
