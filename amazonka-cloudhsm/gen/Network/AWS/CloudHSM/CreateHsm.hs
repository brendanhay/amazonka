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
-- Module      : Network.AWS.CloudHSM.CreateHsm
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
-- Creates an uninitialized HSM instance.
--
-- There is an upfront fee charged for each HSM instance that you create
-- with the @CreateHsm@ operation. If you accidentally provision an HSM and
-- want to request a refund, delete the instance using the DeleteHsm
-- operation, go to the
-- <https://console.aws.amazon.com/support/home AWS Support Center>, create
-- a new case, and select __Account and Billing Support__.
--
-- It can take up to 20 minutes to create and provision an HSM. You can
-- monitor the status of the HSM with the DescribeHsm operation. The HSM is
-- ready to be initialized when the status changes to @RUNNING@.
module Network.AWS.CloudHSM.CreateHsm
  ( -- * Creating a Request
    CreateHsm (..),
    newCreateHsm,

    -- * Request Lenses
    createHsm_eniIp,
    createHsm_syslogIp,
    createHsm_externalId,
    createHsm_clientToken,
    createHsm_subnetId,
    createHsm_sshKey,
    createHsm_iamRoleArn,
    createHsm_subscriptionType,

    -- * Destructuring the Response
    CreateHsmResponse (..),
    newCreateHsmResponse,

    -- * Response Lenses
    createHsmResponse_hsmArn,
    createHsmResponse_httpStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the @CreateHsm@ operation.
--
-- /See:/ 'newCreateHsm' smart constructor.
data CreateHsm = CreateHsm'
  { -- | The IP address to assign to the HSM\'s ENI.
    --
    -- If an IP address is not specified, an IP address will be randomly chosen
    -- from the CIDR range of the subnet.
    eniIp :: Prelude.Maybe Prelude.Text,
    -- | The IP address for the syslog monitoring server. The AWS CloudHSM
    -- service only supports one syslog monitoring server.
    syslogIp :: Prelude.Maybe Prelude.Text,
    -- | The external ID from @IamRoleArn@, if present.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | A user-defined token to ensure idempotence. Subsequent calls to this
    -- operation with the same token will be ignored.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet in your VPC in which to place the HSM.
    subnetId :: Prelude.Text,
    -- | The SSH public key to install on the HSM.
    sshKey :: Prelude.Text,
    -- | The ARN of an IAM role to enable the AWS CloudHSM service to allocate an
    -- ENI on your behalf.
    iamRoleArn :: Prelude.Text,
    subscriptionType :: SubscriptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateHsm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eniIp', 'createHsm_eniIp' - The IP address to assign to the HSM\'s ENI.
--
-- If an IP address is not specified, an IP address will be randomly chosen
-- from the CIDR range of the subnet.
--
-- 'syslogIp', 'createHsm_syslogIp' - The IP address for the syslog monitoring server. The AWS CloudHSM
-- service only supports one syslog monitoring server.
--
-- 'externalId', 'createHsm_externalId' - The external ID from @IamRoleArn@, if present.
--
-- 'clientToken', 'createHsm_clientToken' - A user-defined token to ensure idempotence. Subsequent calls to this
-- operation with the same token will be ignored.
--
-- 'subnetId', 'createHsm_subnetId' - The identifier of the subnet in your VPC in which to place the HSM.
--
-- 'sshKey', 'createHsm_sshKey' - The SSH public key to install on the HSM.
--
-- 'iamRoleArn', 'createHsm_iamRoleArn' - The ARN of an IAM role to enable the AWS CloudHSM service to allocate an
-- ENI on your behalf.
--
-- 'subscriptionType', 'createHsm_subscriptionType' - Undocumented member.
newCreateHsm ::
  -- | 'subnetId'
  Prelude.Text ->
  -- | 'sshKey'
  Prelude.Text ->
  -- | 'iamRoleArn'
  Prelude.Text ->
  -- | 'subscriptionType'
  SubscriptionType ->
  CreateHsm
newCreateHsm
  pSubnetId_
  pSshKey_
  pIamRoleArn_
  pSubscriptionType_ =
    CreateHsm'
      { eniIp = Prelude.Nothing,
        syslogIp = Prelude.Nothing,
        externalId = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        subnetId = pSubnetId_,
        sshKey = pSshKey_,
        iamRoleArn = pIamRoleArn_,
        subscriptionType = pSubscriptionType_
      }

-- | The IP address to assign to the HSM\'s ENI.
--
-- If an IP address is not specified, an IP address will be randomly chosen
-- from the CIDR range of the subnet.
createHsm_eniIp :: Lens.Lens' CreateHsm (Prelude.Maybe Prelude.Text)
createHsm_eniIp = Lens.lens (\CreateHsm' {eniIp} -> eniIp) (\s@CreateHsm' {} a -> s {eniIp = a} :: CreateHsm)

-- | The IP address for the syslog monitoring server. The AWS CloudHSM
-- service only supports one syslog monitoring server.
createHsm_syslogIp :: Lens.Lens' CreateHsm (Prelude.Maybe Prelude.Text)
createHsm_syslogIp = Lens.lens (\CreateHsm' {syslogIp} -> syslogIp) (\s@CreateHsm' {} a -> s {syslogIp = a} :: CreateHsm)

-- | The external ID from @IamRoleArn@, if present.
createHsm_externalId :: Lens.Lens' CreateHsm (Prelude.Maybe Prelude.Text)
createHsm_externalId = Lens.lens (\CreateHsm' {externalId} -> externalId) (\s@CreateHsm' {} a -> s {externalId = a} :: CreateHsm)

-- | A user-defined token to ensure idempotence. Subsequent calls to this
-- operation with the same token will be ignored.
createHsm_clientToken :: Lens.Lens' CreateHsm (Prelude.Maybe Prelude.Text)
createHsm_clientToken = Lens.lens (\CreateHsm' {clientToken} -> clientToken) (\s@CreateHsm' {} a -> s {clientToken = a} :: CreateHsm)

-- | The identifier of the subnet in your VPC in which to place the HSM.
createHsm_subnetId :: Lens.Lens' CreateHsm Prelude.Text
createHsm_subnetId = Lens.lens (\CreateHsm' {subnetId} -> subnetId) (\s@CreateHsm' {} a -> s {subnetId = a} :: CreateHsm)

-- | The SSH public key to install on the HSM.
createHsm_sshKey :: Lens.Lens' CreateHsm Prelude.Text
createHsm_sshKey = Lens.lens (\CreateHsm' {sshKey} -> sshKey) (\s@CreateHsm' {} a -> s {sshKey = a} :: CreateHsm)

-- | The ARN of an IAM role to enable the AWS CloudHSM service to allocate an
-- ENI on your behalf.
createHsm_iamRoleArn :: Lens.Lens' CreateHsm Prelude.Text
createHsm_iamRoleArn = Lens.lens (\CreateHsm' {iamRoleArn} -> iamRoleArn) (\s@CreateHsm' {} a -> s {iamRoleArn = a} :: CreateHsm)

-- | Undocumented member.
createHsm_subscriptionType :: Lens.Lens' CreateHsm SubscriptionType
createHsm_subscriptionType = Lens.lens (\CreateHsm' {subscriptionType} -> subscriptionType) (\s@CreateHsm' {} a -> s {subscriptionType = a} :: CreateHsm)

instance Prelude.AWSRequest CreateHsm where
  type Rs CreateHsm = CreateHsmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHsmResponse'
            Prelude.<$> (x Prelude..?> "HsmArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHsm

instance Prelude.NFData CreateHsm

instance Prelude.ToHeaders CreateHsm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CloudHsmFrontendService.CreateHsm" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateHsm where
  toJSON CreateHsm' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EniIp" Prelude..=) Prelude.<$> eniIp,
            ("SyslogIp" Prelude..=) Prelude.<$> syslogIp,
            ("ExternalId" Prelude..=) Prelude.<$> externalId,
            ("ClientToken" Prelude..=) Prelude.<$> clientToken,
            Prelude.Just ("SubnetId" Prelude..= subnetId),
            Prelude.Just ("SshKey" Prelude..= sshKey),
            Prelude.Just ("IamRoleArn" Prelude..= iamRoleArn),
            Prelude.Just
              ("SubscriptionType" Prelude..= subscriptionType)
          ]
      )

instance Prelude.ToPath CreateHsm where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateHsm where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the @CreateHsm@ operation.
--
-- /See:/ 'newCreateHsmResponse' smart constructor.
data CreateHsmResponse = CreateHsmResponse'
  { -- | The ARN of the HSM.
    hsmArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateHsmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmArn', 'createHsmResponse_hsmArn' - The ARN of the HSM.
--
-- 'httpStatus', 'createHsmResponse_httpStatus' - The response's http status code.
newCreateHsmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateHsmResponse
newCreateHsmResponse pHttpStatus_ =
  CreateHsmResponse'
    { hsmArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the HSM.
createHsmResponse_hsmArn :: Lens.Lens' CreateHsmResponse (Prelude.Maybe Prelude.Text)
createHsmResponse_hsmArn = Lens.lens (\CreateHsmResponse' {hsmArn} -> hsmArn) (\s@CreateHsmResponse' {} a -> s {hsmArn = a} :: CreateHsmResponse)

-- | The response's http status code.
createHsmResponse_httpStatus :: Lens.Lens' CreateHsmResponse Prelude.Int
createHsmResponse_httpStatus = Lens.lens (\CreateHsmResponse' {httpStatus} -> httpStatus) (\s@CreateHsmResponse' {} a -> s {httpStatus = a} :: CreateHsmResponse)

instance Prelude.NFData CreateHsmResponse
