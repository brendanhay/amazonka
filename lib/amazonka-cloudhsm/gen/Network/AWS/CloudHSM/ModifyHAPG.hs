{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyHAPG
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Modifies an existing high-availability partition group.
module Network.AWS.CloudHSM.ModifyHAPG
  ( -- * Creating a request
    ModifyHAPG (..),
    mkModifyHAPG,

    -- ** Request lenses
    mhPartitionSerialList,
    mhLabel,
    mhHAPGARN,

    -- * Destructuring the response
    ModifyHAPGResponse (..),
    mkModifyHAPGResponse,

    -- ** Response lenses
    mhrsHAPGARN,
    mhrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyHAPG' smart constructor.
data ModifyHAPG = ModifyHAPG'
  { partitionSerialList ::
      Lude.Maybe [Lude.Text],
    label :: Lude.Maybe Lude.Text,
    hapgARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyHAPG' with the minimum fields required to make a request.
--
-- * 'hapgARN' - The ARN of the high-availability partition group to modify.
-- * 'label' - The new label for the high-availability partition group.
-- * 'partitionSerialList' - The list of partition serial numbers to make members of the high-availability partition group.
mkModifyHAPG ::
  -- | 'hapgARN'
  Lude.Text ->
  ModifyHAPG
mkModifyHAPG pHAPGARN_ =
  ModifyHAPG'
    { partitionSerialList = Lude.Nothing,
      label = Lude.Nothing,
      hapgARN = pHAPGARN_
    }

-- | The list of partition serial numbers to make members of the high-availability partition group.
--
-- /Note:/ Consider using 'partitionSerialList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhPartitionSerialList :: Lens.Lens' ModifyHAPG (Lude.Maybe [Lude.Text])
mhPartitionSerialList = Lens.lens (partitionSerialList :: ModifyHAPG -> Lude.Maybe [Lude.Text]) (\s a -> s {partitionSerialList = a} :: ModifyHAPG)
{-# DEPRECATED mhPartitionSerialList "Use generic-lens or generic-optics with 'partitionSerialList' instead." #-}

-- | The new label for the high-availability partition group.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhLabel :: Lens.Lens' ModifyHAPG (Lude.Maybe Lude.Text)
mhLabel = Lens.lens (label :: ModifyHAPG -> Lude.Maybe Lude.Text) (\s a -> s {label = a} :: ModifyHAPG)
{-# DEPRECATED mhLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The ARN of the high-availability partition group to modify.
--
-- /Note:/ Consider using 'hapgARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhHAPGARN :: Lens.Lens' ModifyHAPG Lude.Text
mhHAPGARN = Lens.lens (hapgARN :: ModifyHAPG -> Lude.Text) (\s a -> s {hapgARN = a} :: ModifyHAPG)
{-# DEPRECATED mhHAPGARN "Use generic-lens or generic-optics with 'hapgARN' instead." #-}

instance Lude.AWSRequest ModifyHAPG where
  type Rs ModifyHAPG = ModifyHAPGResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyHAPGResponse'
            Lude.<$> (x Lude..?> "HapgArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyHAPG where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.ModifyHapg" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyHAPG where
  toJSON ModifyHAPG' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PartitionSerialList" Lude..=) Lude.<$> partitionSerialList,
            ("Label" Lude..=) Lude.<$> label,
            Lude.Just ("HapgArn" Lude..= hapgARN)
          ]
      )

instance Lude.ToPath ModifyHAPG where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyHAPG where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyHAPGResponse' smart constructor.
data ModifyHAPGResponse = ModifyHAPGResponse'
  { hapgARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ModifyHAPGResponse' with the minimum fields required to make a request.
--
-- * 'hapgARN' - The ARN of the high-availability partition group.
-- * 'responseStatus' - The response status code.
mkModifyHAPGResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyHAPGResponse
mkModifyHAPGResponse pResponseStatus_ =
  ModifyHAPGResponse'
    { hapgARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the high-availability partition group.
--
-- /Note:/ Consider using 'hapgARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrsHAPGARN :: Lens.Lens' ModifyHAPGResponse (Lude.Maybe Lude.Text)
mhrsHAPGARN = Lens.lens (hapgARN :: ModifyHAPGResponse -> Lude.Maybe Lude.Text) (\s a -> s {hapgARN = a} :: ModifyHAPGResponse)
{-# DEPRECATED mhrsHAPGARN "Use generic-lens or generic-optics with 'hapgARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrsResponseStatus :: Lens.Lens' ModifyHAPGResponse Lude.Int
mhrsResponseStatus = Lens.lens (responseStatus :: ModifyHAPGResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyHAPGResponse)
{-# DEPRECATED mhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
