{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyTargetGroupAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attributes of the specified target group.
module Network.AWS.ELBv2.ModifyTargetGroupAttributes
  ( -- * Creating a request
    ModifyTargetGroupAttributes (..),
    mkModifyTargetGroupAttributes,

    -- ** Request lenses
    mtgaTargetGroupARN,
    mtgaAttributes,

    -- * Destructuring the response
    ModifyTargetGroupAttributesResponse (..),
    mkModifyTargetGroupAttributesResponse,

    -- ** Response lenses
    mtgarsAttributes,
    mtgarsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyTargetGroupAttributes' smart constructor.
data ModifyTargetGroupAttributes = ModifyTargetGroupAttributes'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupARN :: Lude.Text,
    -- | The attributes.
    attributes :: [TargetGroupAttribute]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTargetGroupAttributes' with the minimum fields required to make a request.
--
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group.
-- * 'attributes' - The attributes.
mkModifyTargetGroupAttributes ::
  -- | 'targetGroupARN'
  Lude.Text ->
  ModifyTargetGroupAttributes
mkModifyTargetGroupAttributes pTargetGroupARN_ =
  ModifyTargetGroupAttributes'
    { targetGroupARN = pTargetGroupARN_,
      attributes = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgaTargetGroupARN :: Lens.Lens' ModifyTargetGroupAttributes Lude.Text
mtgaTargetGroupARN = Lens.lens (targetGroupARN :: ModifyTargetGroupAttributes -> Lude.Text) (\s a -> s {targetGroupARN = a} :: ModifyTargetGroupAttributes)
{-# DEPRECATED mtgaTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

-- | The attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgaAttributes :: Lens.Lens' ModifyTargetGroupAttributes [TargetGroupAttribute]
mtgaAttributes = Lens.lens (attributes :: ModifyTargetGroupAttributes -> [TargetGroupAttribute]) (\s a -> s {attributes = a} :: ModifyTargetGroupAttributes)
{-# DEPRECATED mtgaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest ModifyTargetGroupAttributes where
  type
    Rs ModifyTargetGroupAttributes =
      ModifyTargetGroupAttributesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "ModifyTargetGroupAttributesResult"
      ( \s h x ->
          ModifyTargetGroupAttributesResponse'
            Lude.<$> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyTargetGroupAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyTargetGroupAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyTargetGroupAttributes where
  toQuery ModifyTargetGroupAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyTargetGroupAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "TargetGroupArn" Lude.=: targetGroupARN,
        "Attributes" Lude.=: Lude.toQueryList "member" attributes
      ]

-- | /See:/ 'mkModifyTargetGroupAttributesResponse' smart constructor.
data ModifyTargetGroupAttributesResponse = ModifyTargetGroupAttributesResponse'
  { -- | Information about the attributes.
    attributes :: Lude.Maybe [TargetGroupAttribute],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTargetGroupAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - Information about the attributes.
-- * 'responseStatus' - The response status code.
mkModifyTargetGroupAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyTargetGroupAttributesResponse
mkModifyTargetGroupAttributesResponse pResponseStatus_ =
  ModifyTargetGroupAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgarsAttributes :: Lens.Lens' ModifyTargetGroupAttributesResponse (Lude.Maybe [TargetGroupAttribute])
mtgarsAttributes = Lens.lens (attributes :: ModifyTargetGroupAttributesResponse -> Lude.Maybe [TargetGroupAttribute]) (\s a -> s {attributes = a} :: ModifyTargetGroupAttributesResponse)
{-# DEPRECATED mtgarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgarsResponseStatus :: Lens.Lens' ModifyTargetGroupAttributesResponse Lude.Int
mtgarsResponseStatus = Lens.lens (responseStatus :: ModifyTargetGroupAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyTargetGroupAttributesResponse)
{-# DEPRECATED mtgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
