{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeDeploy.Types.ErrorInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.ErrorInformation where

import Amazonka.CodeDeploy.Types.DeployErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a deployment error.
--
-- /See:/ 'newErrorInformation' smart constructor.
data ErrorInformation = ErrorInformation'
  { -- | For more information, see
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for CodeDeploy>
    -- in the
    -- <https://docs.aws.amazon.com/codedeploy/latest/userguide CodeDeploy User Guide>.
    --
    -- The error code:
    --
    -- -   APPLICATION_MISSING: The application was missing. This error code is
    --     most likely raised if the application is deleted after the
    --     deployment is created, but before it is started.
    --
    -- -   DEPLOYMENT_GROUP_MISSING: The deployment group was missing. This
    --     error code is most likely raised if the deployment group is deleted
    --     after the deployment is created, but before it is started.
    --
    -- -   HEALTH_CONSTRAINTS: The deployment failed on too many instances to
    --     be successfully deployed within the instance health constraints
    --     specified.
    --
    -- -   HEALTH_CONSTRAINTS_INVALID: The revision cannot be successfully
    --     deployed within the instance health constraints specified.
    --
    -- -   IAM_ROLE_MISSING: The service role cannot be accessed.
    --
    -- -   IAM_ROLE_PERMISSIONS: The service role does not have the correct
    --     permissions.
    --
    -- -   INTERNAL_ERROR: There was an internal error.
    --
    -- -   NO_EC2_SUBSCRIPTION: The calling account is not subscribed to Amazon
    --     EC2.
    --
    -- -   NO_INSTANCES: No instances were specified, or no instances can be
    --     found.
    --
    -- -   OVER_MAX_INSTANCES: The maximum number of instances was exceeded.
    --
    -- -   THROTTLED: The operation was throttled because the calling account
    --     exceeded the throttling limits of one or more Amazon Web Services
    --     services.
    --
    -- -   TIMEOUT: The deployment has timed out.
    --
    -- -   REVISION_MISSING: The revision ID was missing. This error code is
    --     most likely raised if the revision is deleted after the deployment
    --     is created, but before it is started.
    code :: Prelude.Maybe DeployErrorCode,
    -- | An accompanying error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'errorInformation_code' - For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for CodeDeploy>
-- in the
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide CodeDeploy User Guide>.
--
-- The error code:
--
-- -   APPLICATION_MISSING: The application was missing. This error code is
--     most likely raised if the application is deleted after the
--     deployment is created, but before it is started.
--
-- -   DEPLOYMENT_GROUP_MISSING: The deployment group was missing. This
--     error code is most likely raised if the deployment group is deleted
--     after the deployment is created, but before it is started.
--
-- -   HEALTH_CONSTRAINTS: The deployment failed on too many instances to
--     be successfully deployed within the instance health constraints
--     specified.
--
-- -   HEALTH_CONSTRAINTS_INVALID: The revision cannot be successfully
--     deployed within the instance health constraints specified.
--
-- -   IAM_ROLE_MISSING: The service role cannot be accessed.
--
-- -   IAM_ROLE_PERMISSIONS: The service role does not have the correct
--     permissions.
--
-- -   INTERNAL_ERROR: There was an internal error.
--
-- -   NO_EC2_SUBSCRIPTION: The calling account is not subscribed to Amazon
--     EC2.
--
-- -   NO_INSTANCES: No instances were specified, or no instances can be
--     found.
--
-- -   OVER_MAX_INSTANCES: The maximum number of instances was exceeded.
--
-- -   THROTTLED: The operation was throttled because the calling account
--     exceeded the throttling limits of one or more Amazon Web Services
--     services.
--
-- -   TIMEOUT: The deployment has timed out.
--
-- -   REVISION_MISSING: The revision ID was missing. This error code is
--     most likely raised if the revision is deleted after the deployment
--     is created, but before it is started.
--
-- 'message', 'errorInformation_message' - An accompanying error message.
newErrorInformation ::
  ErrorInformation
newErrorInformation =
  ErrorInformation'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide/error-codes.html Error Codes for CodeDeploy>
-- in the
-- <https://docs.aws.amazon.com/codedeploy/latest/userguide CodeDeploy User Guide>.
--
-- The error code:
--
-- -   APPLICATION_MISSING: The application was missing. This error code is
--     most likely raised if the application is deleted after the
--     deployment is created, but before it is started.
--
-- -   DEPLOYMENT_GROUP_MISSING: The deployment group was missing. This
--     error code is most likely raised if the deployment group is deleted
--     after the deployment is created, but before it is started.
--
-- -   HEALTH_CONSTRAINTS: The deployment failed on too many instances to
--     be successfully deployed within the instance health constraints
--     specified.
--
-- -   HEALTH_CONSTRAINTS_INVALID: The revision cannot be successfully
--     deployed within the instance health constraints specified.
--
-- -   IAM_ROLE_MISSING: The service role cannot be accessed.
--
-- -   IAM_ROLE_PERMISSIONS: The service role does not have the correct
--     permissions.
--
-- -   INTERNAL_ERROR: There was an internal error.
--
-- -   NO_EC2_SUBSCRIPTION: The calling account is not subscribed to Amazon
--     EC2.
--
-- -   NO_INSTANCES: No instances were specified, or no instances can be
--     found.
--
-- -   OVER_MAX_INSTANCES: The maximum number of instances was exceeded.
--
-- -   THROTTLED: The operation was throttled because the calling account
--     exceeded the throttling limits of one or more Amazon Web Services
--     services.
--
-- -   TIMEOUT: The deployment has timed out.
--
-- -   REVISION_MISSING: The revision ID was missing. This error code is
--     most likely raised if the revision is deleted after the deployment
--     is created, but before it is started.
errorInformation_code :: Lens.Lens' ErrorInformation (Prelude.Maybe DeployErrorCode)
errorInformation_code = Lens.lens (\ErrorInformation' {code} -> code) (\s@ErrorInformation' {} a -> s {code = a} :: ErrorInformation)

-- | An accompanying error message.
errorInformation_message :: Lens.Lens' ErrorInformation (Prelude.Maybe Prelude.Text)
errorInformation_message = Lens.lens (\ErrorInformation' {message} -> message) (\s@ErrorInformation' {} a -> s {message = a} :: ErrorInformation)

instance Data.FromJSON ErrorInformation where
  parseJSON =
    Data.withObject
      "ErrorInformation"
      ( \x ->
          ErrorInformation'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable ErrorInformation where
  hashWithSalt _salt ErrorInformation' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ErrorInformation where
  rnf ErrorInformation' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
