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
-- Module      : Amazonka.CloudHSM.CreateHsm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
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
module Amazonka.CloudHSM.CreateHsm
  ( -- * Creating a Request
    CreateHsm (..),
    newCreateHsm,

    -- * Request Lenses
    createHsm_clientToken,
    createHsm_eniIp,
    createHsm_externalId,
    createHsm_syslogIp,
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

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the @CreateHsm@ operation.
--
-- /See:/ 'newCreateHsm' smart constructor.
data CreateHsm = CreateHsm'
  { -- | A user-defined token to ensure idempotence. Subsequent calls to this
    -- operation with the same token will be ignored.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The IP address to assign to the HSM\'s ENI.
    --
    -- If an IP address is not specified, an IP address will be randomly chosen
    -- from the CIDR range of the subnet.
    eniIp :: Prelude.Maybe Prelude.Text,
    -- | The external ID from @IamRoleArn@, if present.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The IP address for the syslog monitoring server. The AWS CloudHSM
    -- service only supports one syslog monitoring server.
    syslogIp :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet in your VPC in which to place the HSM.
    subnetId :: Prelude.Text,
    -- | The SSH public key to install on the HSM.
    sshKey :: Prelude.Text,
    -- | The ARN of an IAM role to enable the AWS CloudHSM service to allocate an
    -- ENI on your behalf.
    iamRoleArn :: Prelude.Text,
    subscriptionType :: SubscriptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHsm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createHsm_clientToken' - A user-defined token to ensure idempotence. Subsequent calls to this
-- operation with the same token will be ignored.
--
-- 'eniIp', 'createHsm_eniIp' - The IP address to assign to the HSM\'s ENI.
--
-- If an IP address is not specified, an IP address will be randomly chosen
-- from the CIDR range of the subnet.
--
-- 'externalId', 'createHsm_externalId' - The external ID from @IamRoleArn@, if present.
--
-- 'syslogIp', 'createHsm_syslogIp' - The IP address for the syslog monitoring server. The AWS CloudHSM
-- service only supports one syslog monitoring server.
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
      { clientToken = Prelude.Nothing,
        eniIp = Prelude.Nothing,
        externalId = Prelude.Nothing,
        syslogIp = Prelude.Nothing,
        subnetId = pSubnetId_,
        sshKey = pSshKey_,
        iamRoleArn = pIamRoleArn_,
        subscriptionType = pSubscriptionType_
      }

-- | A user-defined token to ensure idempotence. Subsequent calls to this
-- operation with the same token will be ignored.
createHsm_clientToken :: Lens.Lens' CreateHsm (Prelude.Maybe Prelude.Text)
createHsm_clientToken = Lens.lens (\CreateHsm' {clientToken} -> clientToken) (\s@CreateHsm' {} a -> s {clientToken = a} :: CreateHsm)

-- | The IP address to assign to the HSM\'s ENI.
--
-- If an IP address is not specified, an IP address will be randomly chosen
-- from the CIDR range of the subnet.
createHsm_eniIp :: Lens.Lens' CreateHsm (Prelude.Maybe Prelude.Text)
createHsm_eniIp = Lens.lens (\CreateHsm' {eniIp} -> eniIp) (\s@CreateHsm' {} a -> s {eniIp = a} :: CreateHsm)

-- | The external ID from @IamRoleArn@, if present.
createHsm_externalId :: Lens.Lens' CreateHsm (Prelude.Maybe Prelude.Text)
createHsm_externalId = Lens.lens (\CreateHsm' {externalId} -> externalId) (\s@CreateHsm' {} a -> s {externalId = a} :: CreateHsm)

-- | The IP address for the syslog monitoring server. The AWS CloudHSM
-- service only supports one syslog monitoring server.
createHsm_syslogIp :: Lens.Lens' CreateHsm (Prelude.Maybe Prelude.Text)
createHsm_syslogIp = Lens.lens (\CreateHsm' {syslogIp} -> syslogIp) (\s@CreateHsm' {} a -> s {syslogIp = a} :: CreateHsm)

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

instance Core.AWSRequest CreateHsm where
  type AWSResponse CreateHsm = CreateHsmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHsmResponse'
            Prelude.<$> (x Data..?> "HsmArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHsm where
  hashWithSalt _salt CreateHsm' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` eniIp
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` syslogIp
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` sshKey
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` subscriptionType

instance Prelude.NFData CreateHsm where
  rnf CreateHsm' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf eniIp
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf syslogIp
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf sshKey
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf subscriptionType

instance Data.ToHeaders CreateHsm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.CreateHsm" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateHsm where
  toJSON CreateHsm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("EniIp" Data..=) Prelude.<$> eniIp,
            ("ExternalId" Data..=) Prelude.<$> externalId,
            ("SyslogIp" Data..=) Prelude.<$> syslogIp,
            Prelude.Just ("SubnetId" Data..= subnetId),
            Prelude.Just ("SshKey" Data..= sshKey),
            Prelude.Just ("IamRoleArn" Data..= iamRoleArn),
            Prelude.Just
              ("SubscriptionType" Data..= subscriptionType)
          ]
      )

instance Data.ToPath CreateHsm where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateHsm where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateHsmResponse where
  rnf CreateHsmResponse' {..} =
    Prelude.rnf hsmArn
      `Prelude.seq` Prelude.rnf httpStatus
