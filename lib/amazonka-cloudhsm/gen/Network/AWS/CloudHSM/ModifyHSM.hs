{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyHSM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Modifies an HSM.
-- /Important:/ This operation can result in the HSM being offline for up to 15 minutes while the AWS CloudHSM service is reconfigured. If you are modifying a production HSM, you should ensure that your AWS CloudHSM service is configured for high availability, and consider executing this operation during a maintenance window.
module Network.AWS.CloudHSM.ModifyHSM
  ( -- * Creating a request
    ModifyHSM (..),
    mkModifyHSM,

    -- ** Request lenses
    mhIAMRoleARN,
    mhSubnetId,
    mhSyslogIP,
    mhExternalId,
    mhEniIP,
    mhHSMARN,

    -- * Destructuring the response
    ModifyHSMResponse (..),
    mkModifyHSMResponse,

    -- ** Response lenses
    mhsmrsHSMARN,
    mhsmrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'ModifyHsm' operation.
--
-- /See:/ 'mkModifyHSM' smart constructor.
data ModifyHSM = ModifyHSM'
  { iamRoleARN :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    syslogIP :: Lude.Maybe Lude.Text,
    externalId :: Lude.Maybe Lude.Text,
    eniIP :: Lude.Maybe Lude.Text,
    hsmARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyHSM' with the minimum fields required to make a request.
--
-- * 'eniIP' - The new IP address for the elastic network interface (ENI) attached to the HSM.
--
-- If the HSM is moved to a different subnet, and an IP address is not specified, an IP address will be randomly chosen from the CIDR range of the new subnet.
-- * 'externalId' - The new external ID.
-- * 'hsmARN' - The ARN of the HSM to modify.
-- * 'iamRoleARN' - The new IAM role ARN.
-- * 'subnetId' - The new identifier of the subnet that the HSM is in. The new subnet must be in the same Availability Zone as the current subnet.
-- * 'syslogIP' - The new IP address for the syslog monitoring server. The AWS CloudHSM service only supports one syslog monitoring server.
mkModifyHSM ::
  -- | 'hsmARN'
  Lude.Text ->
  ModifyHSM
mkModifyHSM pHSMARN_ =
  ModifyHSM'
    { iamRoleARN = Lude.Nothing,
      subnetId = Lude.Nothing,
      syslogIP = Lude.Nothing,
      externalId = Lude.Nothing,
      eniIP = Lude.Nothing,
      hsmARN = pHSMARN_
    }

-- | The new IAM role ARN.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhIAMRoleARN :: Lens.Lens' ModifyHSM (Lude.Maybe Lude.Text)
mhIAMRoleARN = Lens.lens (iamRoleARN :: ModifyHSM -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: ModifyHSM)
{-# DEPRECATED mhIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The new identifier of the subnet that the HSM is in. The new subnet must be in the same Availability Zone as the current subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhSubnetId :: Lens.Lens' ModifyHSM (Lude.Maybe Lude.Text)
mhSubnetId = Lens.lens (subnetId :: ModifyHSM -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: ModifyHSM)
{-# DEPRECATED mhSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The new IP address for the syslog monitoring server. The AWS CloudHSM service only supports one syslog monitoring server.
--
-- /Note:/ Consider using 'syslogIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhSyslogIP :: Lens.Lens' ModifyHSM (Lude.Maybe Lude.Text)
mhSyslogIP = Lens.lens (syslogIP :: ModifyHSM -> Lude.Maybe Lude.Text) (\s a -> s {syslogIP = a} :: ModifyHSM)
{-# DEPRECATED mhSyslogIP "Use generic-lens or generic-optics with 'syslogIP' instead." #-}

-- | The new external ID.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhExternalId :: Lens.Lens' ModifyHSM (Lude.Maybe Lude.Text)
mhExternalId = Lens.lens (externalId :: ModifyHSM -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: ModifyHSM)
{-# DEPRECATED mhExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The new IP address for the elastic network interface (ENI) attached to the HSM.
--
-- If the HSM is moved to a different subnet, and an IP address is not specified, an IP address will be randomly chosen from the CIDR range of the new subnet.
--
-- /Note:/ Consider using 'eniIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhEniIP :: Lens.Lens' ModifyHSM (Lude.Maybe Lude.Text)
mhEniIP = Lens.lens (eniIP :: ModifyHSM -> Lude.Maybe Lude.Text) (\s a -> s {eniIP = a} :: ModifyHSM)
{-# DEPRECATED mhEniIP "Use generic-lens or generic-optics with 'eniIP' instead." #-}

-- | The ARN of the HSM to modify.
--
-- /Note:/ Consider using 'hsmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhHSMARN :: Lens.Lens' ModifyHSM Lude.Text
mhHSMARN = Lens.lens (hsmARN :: ModifyHSM -> Lude.Text) (\s a -> s {hsmARN = a} :: ModifyHSM)
{-# DEPRECATED mhHSMARN "Use generic-lens or generic-optics with 'hsmARN' instead." #-}

instance Lude.AWSRequest ModifyHSM where
  type Rs ModifyHSM = ModifyHSMResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyHSMResponse'
            Lude.<$> (x Lude..?> "HsmArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyHSM where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.ModifyHsm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyHSM where
  toJSON ModifyHSM' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IamRoleArn" Lude..=) Lude.<$> iamRoleARN,
            ("SubnetId" Lude..=) Lude.<$> subnetId,
            ("SyslogIp" Lude..=) Lude.<$> syslogIP,
            ("ExternalId" Lude..=) Lude.<$> externalId,
            ("EniIp" Lude..=) Lude.<$> eniIP,
            Lude.Just ("HsmArn" Lude..= hsmARN)
          ]
      )

instance Lude.ToPath ModifyHSM where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyHSM where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of the 'ModifyHsm' operation.
--
-- /See:/ 'mkModifyHSMResponse' smart constructor.
data ModifyHSMResponse = ModifyHSMResponse'
  { hsmARN ::
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

-- | Creates a value of 'ModifyHSMResponse' with the minimum fields required to make a request.
--
-- * 'hsmARN' - The ARN of the HSM.
-- * 'responseStatus' - The response status code.
mkModifyHSMResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyHSMResponse
mkModifyHSMResponse pResponseStatus_ =
  ModifyHSMResponse'
    { hsmARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the HSM.
--
-- /Note:/ Consider using 'hsmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhsmrsHSMARN :: Lens.Lens' ModifyHSMResponse (Lude.Maybe Lude.Text)
mhsmrsHSMARN = Lens.lens (hsmARN :: ModifyHSMResponse -> Lude.Maybe Lude.Text) (\s a -> s {hsmARN = a} :: ModifyHSMResponse)
{-# DEPRECATED mhsmrsHSMARN "Use generic-lens or generic-optics with 'hsmARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhsmrsResponseStatus :: Lens.Lens' ModifyHSMResponse Lude.Int
mhsmrsResponseStatus = Lens.lens (responseStatus :: ModifyHSMResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyHSMResponse)
{-# DEPRECATED mhsmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
