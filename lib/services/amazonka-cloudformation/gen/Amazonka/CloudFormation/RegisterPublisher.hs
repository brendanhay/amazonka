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
-- Module      : Amazonka.CloudFormation.RegisterPublisher
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers your account as a publisher of public extensions in the
-- CloudFormation registry. Public extensions are available for use by all
-- CloudFormation users. This publisher ID applies to your account in all
-- Amazon Web Services Regions.
--
-- For information about requirements for registering as a public extension
-- publisher, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/publish-extension.html#publish-extension-prereqs Registering your account to publish CloudFormation extensions>
-- in the /CloudFormation CLI User Guide/.
module Amazonka.CloudFormation.RegisterPublisher
  ( -- * Creating a Request
    RegisterPublisher (..),
    newRegisterPublisher,

    -- * Request Lenses
    registerPublisher_acceptTermsAndConditions,
    registerPublisher_connectionArn,

    -- * Destructuring the Response
    RegisterPublisherResponse (..),
    newRegisterPublisherResponse,

    -- * Response Lenses
    registerPublisherResponse_publisherId,
    registerPublisherResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterPublisher' smart constructor.
data RegisterPublisher = RegisterPublisher'
  { -- | Whether you accept the
    -- <https://cloudformation-registry-documents.s3.amazonaws.com/Terms_and_Conditions_for_AWS_CloudFormation_Registry_Publishers.pdf Terms and Conditions>
    -- for publishing extensions in the CloudFormation registry. You must
    -- accept the terms and conditions in order to register to publish public
    -- extensions to the CloudFormation registry.
    --
    -- The default is @false@.
    acceptTermsAndConditions :: Prelude.Maybe Prelude.Bool,
    -- | If you are using a Bitbucket or GitHub account for identity
    -- verification, the Amazon Resource Name (ARN) for your connection to that
    -- account.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/publish-extension.html#publish-extension-prereqs Registering your account to publish CloudFormation extensions>
    -- in the /CloudFormation CLI User Guide/.
    connectionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterPublisher' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptTermsAndConditions', 'registerPublisher_acceptTermsAndConditions' - Whether you accept the
-- <https://cloudformation-registry-documents.s3.amazonaws.com/Terms_and_Conditions_for_AWS_CloudFormation_Registry_Publishers.pdf Terms and Conditions>
-- for publishing extensions in the CloudFormation registry. You must
-- accept the terms and conditions in order to register to publish public
-- extensions to the CloudFormation registry.
--
-- The default is @false@.
--
-- 'connectionArn', 'registerPublisher_connectionArn' - If you are using a Bitbucket or GitHub account for identity
-- verification, the Amazon Resource Name (ARN) for your connection to that
-- account.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/publish-extension.html#publish-extension-prereqs Registering your account to publish CloudFormation extensions>
-- in the /CloudFormation CLI User Guide/.
newRegisterPublisher ::
  RegisterPublisher
newRegisterPublisher =
  RegisterPublisher'
    { acceptTermsAndConditions =
        Prelude.Nothing,
      connectionArn = Prelude.Nothing
    }

-- | Whether you accept the
-- <https://cloudformation-registry-documents.s3.amazonaws.com/Terms_and_Conditions_for_AWS_CloudFormation_Registry_Publishers.pdf Terms and Conditions>
-- for publishing extensions in the CloudFormation registry. You must
-- accept the terms and conditions in order to register to publish public
-- extensions to the CloudFormation registry.
--
-- The default is @false@.
registerPublisher_acceptTermsAndConditions :: Lens.Lens' RegisterPublisher (Prelude.Maybe Prelude.Bool)
registerPublisher_acceptTermsAndConditions = Lens.lens (\RegisterPublisher' {acceptTermsAndConditions} -> acceptTermsAndConditions) (\s@RegisterPublisher' {} a -> s {acceptTermsAndConditions = a} :: RegisterPublisher)

-- | If you are using a Bitbucket or GitHub account for identity
-- verification, the Amazon Resource Name (ARN) for your connection to that
-- account.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/publish-extension.html#publish-extension-prereqs Registering your account to publish CloudFormation extensions>
-- in the /CloudFormation CLI User Guide/.
registerPublisher_connectionArn :: Lens.Lens' RegisterPublisher (Prelude.Maybe Prelude.Text)
registerPublisher_connectionArn = Lens.lens (\RegisterPublisher' {connectionArn} -> connectionArn) (\s@RegisterPublisher' {} a -> s {connectionArn = a} :: RegisterPublisher)

instance Core.AWSRequest RegisterPublisher where
  type
    AWSResponse RegisterPublisher =
      RegisterPublisherResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RegisterPublisherResult"
      ( \s h x ->
          RegisterPublisherResponse'
            Prelude.<$> (x Data..@? "PublisherId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterPublisher where
  hashWithSalt _salt RegisterPublisher' {..} =
    _salt
      `Prelude.hashWithSalt` acceptTermsAndConditions
      `Prelude.hashWithSalt` connectionArn

instance Prelude.NFData RegisterPublisher where
  rnf RegisterPublisher' {..} =
    Prelude.rnf acceptTermsAndConditions `Prelude.seq`
      Prelude.rnf connectionArn

instance Data.ToHeaders RegisterPublisher where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RegisterPublisher where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterPublisher where
  toQuery RegisterPublisher' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RegisterPublisher" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "AcceptTermsAndConditions"
          Data.=: acceptTermsAndConditions,
        "ConnectionArn" Data.=: connectionArn
      ]

-- | /See:/ 'newRegisterPublisherResponse' smart constructor.
data RegisterPublisherResponse = RegisterPublisherResponse'
  { -- | The ID assigned this account by CloudFormation for publishing
    -- extensions.
    publisherId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterPublisherResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publisherId', 'registerPublisherResponse_publisherId' - The ID assigned this account by CloudFormation for publishing
-- extensions.
--
-- 'httpStatus', 'registerPublisherResponse_httpStatus' - The response's http status code.
newRegisterPublisherResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterPublisherResponse
newRegisterPublisherResponse pHttpStatus_ =
  RegisterPublisherResponse'
    { publisherId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID assigned this account by CloudFormation for publishing
-- extensions.
registerPublisherResponse_publisherId :: Lens.Lens' RegisterPublisherResponse (Prelude.Maybe Prelude.Text)
registerPublisherResponse_publisherId = Lens.lens (\RegisterPublisherResponse' {publisherId} -> publisherId) (\s@RegisterPublisherResponse' {} a -> s {publisherId = a} :: RegisterPublisherResponse)

-- | The response's http status code.
registerPublisherResponse_httpStatus :: Lens.Lens' RegisterPublisherResponse Prelude.Int
registerPublisherResponse_httpStatus = Lens.lens (\RegisterPublisherResponse' {httpStatus} -> httpStatus) (\s@RegisterPublisherResponse' {} a -> s {httpStatus = a} :: RegisterPublisherResponse)

instance Prelude.NFData RegisterPublisherResponse where
  rnf RegisterPublisherResponse' {..} =
    Prelude.rnf publisherId `Prelude.seq`
      Prelude.rnf httpStatus
