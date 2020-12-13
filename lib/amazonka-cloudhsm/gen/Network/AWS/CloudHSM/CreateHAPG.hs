{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.CreateHAPG
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Creates a high-availability partition group. A high-availability partition group is a group of partitions that spans multiple physical HSMs.
module Network.AWS.CloudHSM.CreateHAPG
  ( -- * Creating a request
    CreateHAPG (..),
    mkCreateHAPG,

    -- ** Request lenses
    chLabel,

    -- * Destructuring the response
    CreateHAPGResponse (..),
    mkCreateHAPGResponse,

    -- ** Response lenses
    chrsHAPGARN,
    chrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'CreateHapgRequest' action.
--
-- /See:/ 'mkCreateHAPG' smart constructor.
newtype CreateHAPG = CreateHAPG'
  { -- | The label of the new high-availability partition group.
    label :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHAPG' with the minimum fields required to make a request.
--
-- * 'label' - The label of the new high-availability partition group.
mkCreateHAPG ::
  -- | 'label'
  Lude.Text ->
  CreateHAPG
mkCreateHAPG pLabel_ = CreateHAPG' {label = pLabel_}

-- | The label of the new high-availability partition group.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chLabel :: Lens.Lens' CreateHAPG Lude.Text
chLabel = Lens.lens (label :: CreateHAPG -> Lude.Text) (\s a -> s {label = a} :: CreateHAPG)
{-# DEPRECATED chLabel "Use generic-lens or generic-optics with 'label' instead." #-}

instance Lude.AWSRequest CreateHAPG where
  type Rs CreateHAPG = CreateHAPGResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateHAPGResponse'
            Lude.<$> (x Lude..?> "HapgArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateHAPG where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.CreateHapg" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateHAPG where
  toJSON CreateHAPG' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Label" Lude..= label)])

instance Lude.ToPath CreateHAPG where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateHAPG where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of the 'CreateHAPartitionGroup' action.
--
-- /See:/ 'mkCreateHAPGResponse' smart constructor.
data CreateHAPGResponse = CreateHAPGResponse'
  { -- | The ARN of the high-availability partition group.
    hapgARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHAPGResponse' with the minimum fields required to make a request.
--
-- * 'hapgARN' - The ARN of the high-availability partition group.
-- * 'responseStatus' - The response status code.
mkCreateHAPGResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateHAPGResponse
mkCreateHAPGResponse pResponseStatus_ =
  CreateHAPGResponse'
    { hapgARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the high-availability partition group.
--
-- /Note:/ Consider using 'hapgARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrsHAPGARN :: Lens.Lens' CreateHAPGResponse (Lude.Maybe Lude.Text)
chrsHAPGARN = Lens.lens (hapgARN :: CreateHAPGResponse -> Lude.Maybe Lude.Text) (\s a -> s {hapgARN = a} :: CreateHAPGResponse)
{-# DEPRECATED chrsHAPGARN "Use generic-lens or generic-optics with 'hapgARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrsResponseStatus :: Lens.Lens' CreateHAPGResponse Lude.Int
chrsResponseStatus = Lens.lens (responseStatus :: CreateHAPGResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHAPGResponse)
{-# DEPRECATED chrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
