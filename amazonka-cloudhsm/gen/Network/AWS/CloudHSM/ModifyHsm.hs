{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyHsm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Modifies an HSM.
--
-- This operation can result in the HSM being offline for up to 15 minutes
-- while the AWS CloudHSM service is reconfigured. If you are modifying a
-- production HSM, you should ensure that your AWS CloudHSM service is
-- configured for high availability, and consider executing this operation
-- during a maintenance window.
module Network.AWS.CloudHSM.ModifyHsm
  ( -- * Creating a Request
    ModifyHsm (..),
    newModifyHsm,

    -- * Request Lenses
    modifyHsm_iamRoleArn,
    modifyHsm_eniIp,
    modifyHsm_syslogIp,
    modifyHsm_subnetId,
    modifyHsm_externalId,
    modifyHsm_hsmArn,

    -- * Destructuring the Response
    ModifyHsmResponse (..),
    newModifyHsmResponse,

    -- * Response Lenses
    modifyHsmResponse_hsmArn,
    modifyHsmResponse_httpStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the ModifyHsm operation.
--
-- /See:/ 'newModifyHsm' smart constructor.
data ModifyHsm = ModifyHsm'
  { -- | The new IAM role ARN.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The new IP address for the elastic network interface (ENI) attached to
    -- the HSM.
    --
    -- If the HSM is moved to a different subnet, and an IP address is not
    -- specified, an IP address will be randomly chosen from the CIDR range of
    -- the new subnet.
    eniIp :: Prelude.Maybe Prelude.Text,
    -- | The new IP address for the syslog monitoring server. The AWS CloudHSM
    -- service only supports one syslog monitoring server.
    syslogIp :: Prelude.Maybe Prelude.Text,
    -- | The new identifier of the subnet that the HSM is in. The new subnet must
    -- be in the same Availability Zone as the current subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The new external ID.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the HSM to modify.
    hsmArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyHsm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamRoleArn', 'modifyHsm_iamRoleArn' - The new IAM role ARN.
--
-- 'eniIp', 'modifyHsm_eniIp' - The new IP address for the elastic network interface (ENI) attached to
-- the HSM.
--
-- If the HSM is moved to a different subnet, and an IP address is not
-- specified, an IP address will be randomly chosen from the CIDR range of
-- the new subnet.
--
-- 'syslogIp', 'modifyHsm_syslogIp' - The new IP address for the syslog monitoring server. The AWS CloudHSM
-- service only supports one syslog monitoring server.
--
-- 'subnetId', 'modifyHsm_subnetId' - The new identifier of the subnet that the HSM is in. The new subnet must
-- be in the same Availability Zone as the current subnet.
--
-- 'externalId', 'modifyHsm_externalId' - The new external ID.
--
-- 'hsmArn', 'modifyHsm_hsmArn' - The ARN of the HSM to modify.
newModifyHsm ::
  -- | 'hsmArn'
  Prelude.Text ->
  ModifyHsm
newModifyHsm pHsmArn_ =
  ModifyHsm'
    { iamRoleArn = Prelude.Nothing,
      eniIp = Prelude.Nothing,
      syslogIp = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      externalId = Prelude.Nothing,
      hsmArn = pHsmArn_
    }

-- | The new IAM role ARN.
modifyHsm_iamRoleArn :: Lens.Lens' ModifyHsm (Prelude.Maybe Prelude.Text)
modifyHsm_iamRoleArn = Lens.lens (\ModifyHsm' {iamRoleArn} -> iamRoleArn) (\s@ModifyHsm' {} a -> s {iamRoleArn = a} :: ModifyHsm)

-- | The new IP address for the elastic network interface (ENI) attached to
-- the HSM.
--
-- If the HSM is moved to a different subnet, and an IP address is not
-- specified, an IP address will be randomly chosen from the CIDR range of
-- the new subnet.
modifyHsm_eniIp :: Lens.Lens' ModifyHsm (Prelude.Maybe Prelude.Text)
modifyHsm_eniIp = Lens.lens (\ModifyHsm' {eniIp} -> eniIp) (\s@ModifyHsm' {} a -> s {eniIp = a} :: ModifyHsm)

-- | The new IP address for the syslog monitoring server. The AWS CloudHSM
-- service only supports one syslog monitoring server.
modifyHsm_syslogIp :: Lens.Lens' ModifyHsm (Prelude.Maybe Prelude.Text)
modifyHsm_syslogIp = Lens.lens (\ModifyHsm' {syslogIp} -> syslogIp) (\s@ModifyHsm' {} a -> s {syslogIp = a} :: ModifyHsm)

-- | The new identifier of the subnet that the HSM is in. The new subnet must
-- be in the same Availability Zone as the current subnet.
modifyHsm_subnetId :: Lens.Lens' ModifyHsm (Prelude.Maybe Prelude.Text)
modifyHsm_subnetId = Lens.lens (\ModifyHsm' {subnetId} -> subnetId) (\s@ModifyHsm' {} a -> s {subnetId = a} :: ModifyHsm)

-- | The new external ID.
modifyHsm_externalId :: Lens.Lens' ModifyHsm (Prelude.Maybe Prelude.Text)
modifyHsm_externalId = Lens.lens (\ModifyHsm' {externalId} -> externalId) (\s@ModifyHsm' {} a -> s {externalId = a} :: ModifyHsm)

-- | The ARN of the HSM to modify.
modifyHsm_hsmArn :: Lens.Lens' ModifyHsm Prelude.Text
modifyHsm_hsmArn = Lens.lens (\ModifyHsm' {hsmArn} -> hsmArn) (\s@ModifyHsm' {} a -> s {hsmArn = a} :: ModifyHsm)

instance Prelude.AWSRequest ModifyHsm where
  type Rs ModifyHsm = ModifyHsmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyHsmResponse'
            Prelude.<$> (x Prelude..?> "HsmArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyHsm

instance Prelude.NFData ModifyHsm

instance Prelude.ToHeaders ModifyHsm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CloudHsmFrontendService.ModifyHsm" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ModifyHsm where
  toJSON ModifyHsm' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IamRoleArn" Prelude..=) Prelude.<$> iamRoleArn,
            ("EniIp" Prelude..=) Prelude.<$> eniIp,
            ("SyslogIp" Prelude..=) Prelude.<$> syslogIp,
            ("SubnetId" Prelude..=) Prelude.<$> subnetId,
            ("ExternalId" Prelude..=) Prelude.<$> externalId,
            Prelude.Just ("HsmArn" Prelude..= hsmArn)
          ]
      )

instance Prelude.ToPath ModifyHsm where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyHsm where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the ModifyHsm operation.
--
-- /See:/ 'newModifyHsmResponse' smart constructor.
data ModifyHsmResponse = ModifyHsmResponse'
  { -- | The ARN of the HSM.
    hsmArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyHsmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmArn', 'modifyHsmResponse_hsmArn' - The ARN of the HSM.
--
-- 'httpStatus', 'modifyHsmResponse_httpStatus' - The response's http status code.
newModifyHsmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyHsmResponse
newModifyHsmResponse pHttpStatus_ =
  ModifyHsmResponse'
    { hsmArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the HSM.
modifyHsmResponse_hsmArn :: Lens.Lens' ModifyHsmResponse (Prelude.Maybe Prelude.Text)
modifyHsmResponse_hsmArn = Lens.lens (\ModifyHsmResponse' {hsmArn} -> hsmArn) (\s@ModifyHsmResponse' {} a -> s {hsmArn = a} :: ModifyHsmResponse)

-- | The response's http status code.
modifyHsmResponse_httpStatus :: Lens.Lens' ModifyHsmResponse Prelude.Int
modifyHsmResponse_httpStatus = Lens.lens (\ModifyHsmResponse' {httpStatus} -> httpStatus) (\s@ModifyHsmResponse' {} a -> s {httpStatus = a} :: ModifyHsmResponse)

instance Prelude.NFData ModifyHsmResponse
