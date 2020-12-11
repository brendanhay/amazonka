{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details of a 'Protection' object.
module Network.AWS.Shield.DescribeProtection
  ( -- * Creating a request
    DescribeProtection (..),
    mkDescribeProtection,

    -- ** Request lenses
    dpProtectionId,
    dpResourceARN,

    -- * Destructuring the response
    DescribeProtectionResponse (..),
    mkDescribeProtectionResponse,

    -- ** Response lenses
    dprsProtection,
    dprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDescribeProtection' smart constructor.
data DescribeProtection = DescribeProtection'
  { protectionId ::
      Lude.Maybe Lude.Text,
    resourceARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProtection' with the minimum fields required to make a request.
--
-- * 'protectionId' - The unique identifier (ID) for the 'Protection' object that is described. When submitting the @DescribeProtection@ request you must provide either the @ResourceArn@ or the @ProtectionID@ , but not both.
-- * 'resourceARN' - The ARN (Amazon Resource Name) of the AWS resource for the 'Protection' object that is described. When submitting the @DescribeProtection@ request you must provide either the @ResourceArn@ or the @ProtectionID@ , but not both.
mkDescribeProtection ::
  DescribeProtection
mkDescribeProtection =
  DescribeProtection'
    { protectionId = Lude.Nothing,
      resourceARN = Lude.Nothing
    }

-- | The unique identifier (ID) for the 'Protection' object that is described. When submitting the @DescribeProtection@ request you must provide either the @ResourceArn@ or the @ProtectionID@ , but not both.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProtectionId :: Lens.Lens' DescribeProtection (Lude.Maybe Lude.Text)
dpProtectionId = Lens.lens (protectionId :: DescribeProtection -> Lude.Maybe Lude.Text) (\s a -> s {protectionId = a} :: DescribeProtection)
{-# DEPRECATED dpProtectionId "Use generic-lens or generic-optics with 'protectionId' instead." #-}

-- | The ARN (Amazon Resource Name) of the AWS resource for the 'Protection' object that is described. When submitting the @DescribeProtection@ request you must provide either the @ResourceArn@ or the @ProtectionID@ , but not both.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpResourceARN :: Lens.Lens' DescribeProtection (Lude.Maybe Lude.Text)
dpResourceARN = Lens.lens (resourceARN :: DescribeProtection -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: DescribeProtection)
{-# DEPRECATED dpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest DescribeProtection where
  type Rs DescribeProtection = DescribeProtectionResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProtectionResponse'
            Lude.<$> (x Lude..?> "Protection") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProtection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DescribeProtection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProtection where
  toJSON DescribeProtection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProtectionId" Lude..=) Lude.<$> protectionId,
            ("ResourceArn" Lude..=) Lude.<$> resourceARN
          ]
      )

instance Lude.ToPath DescribeProtection where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProtection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProtectionResponse' smart constructor.
data DescribeProtectionResponse = DescribeProtectionResponse'
  { protection ::
      Lude.Maybe Protection,
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

-- | Creates a value of 'DescribeProtectionResponse' with the minimum fields required to make a request.
--
-- * 'protection' - The 'Protection' object that is described.
-- * 'responseStatus' - The response status code.
mkDescribeProtectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProtectionResponse
mkDescribeProtectionResponse pResponseStatus_ =
  DescribeProtectionResponse'
    { protection = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'Protection' object that is described.
--
-- /Note:/ Consider using 'protection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsProtection :: Lens.Lens' DescribeProtectionResponse (Lude.Maybe Protection)
dprsProtection = Lens.lens (protection :: DescribeProtectionResponse -> Lude.Maybe Protection) (\s a -> s {protection = a} :: DescribeProtectionResponse)
{-# DEPRECATED dprsProtection "Use generic-lens or generic-optics with 'protection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DescribeProtectionResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DescribeProtectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProtectionResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
