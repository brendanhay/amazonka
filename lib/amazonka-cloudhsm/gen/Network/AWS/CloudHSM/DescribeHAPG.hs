{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeHAPG
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Retrieves information about a high-availability partition group.
module Network.AWS.CloudHSM.DescribeHAPG
  ( -- * Creating a request
    DescribeHAPG (..),
    mkDescribeHAPG,

    -- ** Request lenses
    dhapgHAPGARN,

    -- * Destructuring the response
    DescribeHAPGResponse (..),
    mkDescribeHAPGResponse,

    -- ** Response lenses
    dhapgrsState,
    dhapgrsLastModifiedTimestamp,
    dhapgrsHSMsPendingRegistration,
    dhapgrsHSMsPendingDeletion,
    dhapgrsHAPGSerial,
    dhapgrsHSMsLastActionFailed,
    dhapgrsPartitionSerialList,
    dhapgrsHAPGARN,
    dhapgrsLabel,
    dhapgrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DescribeHapg' action.
--
-- /See:/ 'mkDescribeHAPG' smart constructor.
newtype DescribeHAPG = DescribeHAPG' {hapgARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHAPG' with the minimum fields required to make a request.
--
-- * 'hapgARN' - The ARN of the high-availability partition group to describe.
mkDescribeHAPG ::
  -- | 'hapgARN'
  Lude.Text ->
  DescribeHAPG
mkDescribeHAPG pHAPGARN_ = DescribeHAPG' {hapgARN = pHAPGARN_}

-- | The ARN of the high-availability partition group to describe.
--
-- /Note:/ Consider using 'hapgARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgHAPGARN :: Lens.Lens' DescribeHAPG Lude.Text
dhapgHAPGARN = Lens.lens (hapgARN :: DescribeHAPG -> Lude.Text) (\s a -> s {hapgARN = a} :: DescribeHAPG)
{-# DEPRECATED dhapgHAPGARN "Use generic-lens or generic-optics with 'hapgARN' instead." #-}

instance Lude.AWSRequest DescribeHAPG where
  type Rs DescribeHAPG = DescribeHAPGResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeHAPGResponse'
            Lude.<$> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "LastModifiedTimestamp")
            Lude.<*> (x Lude..?> "HsmsPendingRegistration" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "HsmsPendingDeletion" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "HapgSerial")
            Lude.<*> (x Lude..?> "HsmsLastActionFailed" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "PartitionSerialList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "HapgArn")
            Lude.<*> (x Lude..?> "Label")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHAPG where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.DescribeHapg" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeHAPG where
  toJSON DescribeHAPG' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("HapgArn" Lude..= hapgARN)])

instance Lude.ToPath DescribeHAPG where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHAPG where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of the 'DescribeHapg' action.
--
-- /See:/ 'mkDescribeHAPGResponse' smart constructor.
data DescribeHAPGResponse = DescribeHAPGResponse'
  { state ::
      Lude.Maybe CloudHSMObjectState,
    lastModifiedTimestamp :: Lude.Maybe Lude.Text,
    hsmsPendingRegistration :: Lude.Maybe [Lude.Text],
    hsmsPendingDeletion :: Lude.Maybe [Lude.Text],
    hapgSerial :: Lude.Maybe Lude.Text,
    hsmsLastActionFailed :: Lude.Maybe [Lude.Text],
    partitionSerialList :: Lude.Maybe [Lude.Text],
    hapgARN :: Lude.Maybe Lude.Text,
    label :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeHAPGResponse' with the minimum fields required to make a request.
--
-- * 'hapgARN' - The ARN of the high-availability partition group.
-- * 'hapgSerial' - The serial number of the high-availability partition group.
-- * 'hsmsLastActionFailed' -
-- * 'hsmsPendingDeletion' -
-- * 'hsmsPendingRegistration' -
-- * 'label' - The label for the high-availability partition group.
-- * 'lastModifiedTimestamp' - The date and time the high-availability partition group was last modified.
-- * 'partitionSerialList' - The list of partition serial numbers that belong to the high-availability partition group.
-- * 'responseStatus' - The response status code.
-- * 'state' - The state of the high-availability partition group.
mkDescribeHAPGResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHAPGResponse
mkDescribeHAPGResponse pResponseStatus_ =
  DescribeHAPGResponse'
    { state = Lude.Nothing,
      lastModifiedTimestamp = Lude.Nothing,
      hsmsPendingRegistration = Lude.Nothing,
      hsmsPendingDeletion = Lude.Nothing,
      hapgSerial = Lude.Nothing,
      hsmsLastActionFailed = Lude.Nothing,
      partitionSerialList = Lude.Nothing,
      hapgARN = Lude.Nothing,
      label = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the high-availability partition group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsState :: Lens.Lens' DescribeHAPGResponse (Lude.Maybe CloudHSMObjectState)
dhapgrsState = Lens.lens (state :: DescribeHAPGResponse -> Lude.Maybe CloudHSMObjectState) (\s a -> s {state = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date and time the high-availability partition group was last modified.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsLastModifiedTimestamp :: Lens.Lens' DescribeHAPGResponse (Lude.Maybe Lude.Text)
dhapgrsLastModifiedTimestamp = Lens.lens (lastModifiedTimestamp :: DescribeHAPGResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedTimestamp = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsLastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead." #-}

-- |
--
-- /Note:/ Consider using 'hsmsPendingRegistration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsHSMsPendingRegistration :: Lens.Lens' DescribeHAPGResponse (Lude.Maybe [Lude.Text])
dhapgrsHSMsPendingRegistration = Lens.lens (hsmsPendingRegistration :: DescribeHAPGResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {hsmsPendingRegistration = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsHSMsPendingRegistration "Use generic-lens or generic-optics with 'hsmsPendingRegistration' instead." #-}

-- |
--
-- /Note:/ Consider using 'hsmsPendingDeletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsHSMsPendingDeletion :: Lens.Lens' DescribeHAPGResponse (Lude.Maybe [Lude.Text])
dhapgrsHSMsPendingDeletion = Lens.lens (hsmsPendingDeletion :: DescribeHAPGResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {hsmsPendingDeletion = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsHSMsPendingDeletion "Use generic-lens or generic-optics with 'hsmsPendingDeletion' instead." #-}

-- | The serial number of the high-availability partition group.
--
-- /Note:/ Consider using 'hapgSerial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsHAPGSerial :: Lens.Lens' DescribeHAPGResponse (Lude.Maybe Lude.Text)
dhapgrsHAPGSerial = Lens.lens (hapgSerial :: DescribeHAPGResponse -> Lude.Maybe Lude.Text) (\s a -> s {hapgSerial = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsHAPGSerial "Use generic-lens or generic-optics with 'hapgSerial' instead." #-}

-- |
--
-- /Note:/ Consider using 'hsmsLastActionFailed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsHSMsLastActionFailed :: Lens.Lens' DescribeHAPGResponse (Lude.Maybe [Lude.Text])
dhapgrsHSMsLastActionFailed = Lens.lens (hsmsLastActionFailed :: DescribeHAPGResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {hsmsLastActionFailed = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsHSMsLastActionFailed "Use generic-lens or generic-optics with 'hsmsLastActionFailed' instead." #-}

-- | The list of partition serial numbers that belong to the high-availability partition group.
--
-- /Note:/ Consider using 'partitionSerialList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsPartitionSerialList :: Lens.Lens' DescribeHAPGResponse (Lude.Maybe [Lude.Text])
dhapgrsPartitionSerialList = Lens.lens (partitionSerialList :: DescribeHAPGResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {partitionSerialList = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsPartitionSerialList "Use generic-lens or generic-optics with 'partitionSerialList' instead." #-}

-- | The ARN of the high-availability partition group.
--
-- /Note:/ Consider using 'hapgARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsHAPGARN :: Lens.Lens' DescribeHAPGResponse (Lude.Maybe Lude.Text)
dhapgrsHAPGARN = Lens.lens (hapgARN :: DescribeHAPGResponse -> Lude.Maybe Lude.Text) (\s a -> s {hapgARN = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsHAPGARN "Use generic-lens or generic-optics with 'hapgARN' instead." #-}

-- | The label for the high-availability partition group.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsLabel :: Lens.Lens' DescribeHAPGResponse (Lude.Maybe Lude.Text)
dhapgrsLabel = Lens.lens (label :: DescribeHAPGResponse -> Lude.Maybe Lude.Text) (\s a -> s {label = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhapgrsResponseStatus :: Lens.Lens' DescribeHAPGResponse Lude.Int
dhapgrsResponseStatus = Lens.lens (responseStatus :: DescribeHAPGResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHAPGResponse)
{-# DEPRECATED dhapgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
