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
-- Module      : Amazonka.DMS.Types.DynamoDbSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.DynamoDbSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the Amazon Resource Name (ARN) of the Identity and Access
-- Management (IAM) role used to define an Amazon DynamoDB target endpoint.
--
-- /See:/ 'newDynamoDbSettings' smart constructor.
data DynamoDbSettings = DynamoDbSettings'
  { -- | The Amazon Resource Name (ARN) used by the service to access the IAM
    -- role. The role must allow the @iam:PassRole@ action.
    serviceAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamoDbSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceAccessRoleArn', 'dynamoDbSettings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) used by the service to access the IAM
-- role. The role must allow the @iam:PassRole@ action.
newDynamoDbSettings ::
  -- | 'serviceAccessRoleArn'
  Prelude.Text ->
  DynamoDbSettings
newDynamoDbSettings pServiceAccessRoleArn_ =
  DynamoDbSettings'
    { serviceAccessRoleArn =
        pServiceAccessRoleArn_
    }

-- | The Amazon Resource Name (ARN) used by the service to access the IAM
-- role. The role must allow the @iam:PassRole@ action.
dynamoDbSettings_serviceAccessRoleArn :: Lens.Lens' DynamoDbSettings Prelude.Text
dynamoDbSettings_serviceAccessRoleArn = Lens.lens (\DynamoDbSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@DynamoDbSettings' {} a -> s {serviceAccessRoleArn = a} :: DynamoDbSettings)

instance Core.FromJSON DynamoDbSettings where
  parseJSON =
    Core.withObject
      "DynamoDbSettings"
      ( \x ->
          DynamoDbSettings'
            Prelude.<$> (x Core..: "ServiceAccessRoleArn")
      )

instance Prelude.Hashable DynamoDbSettings where
  hashWithSalt _salt DynamoDbSettings' {..} =
    _salt `Prelude.hashWithSalt` serviceAccessRoleArn

instance Prelude.NFData DynamoDbSettings where
  rnf DynamoDbSettings' {..} =
    Prelude.rnf serviceAccessRoleArn

instance Core.ToJSON DynamoDbSettings where
  toJSON DynamoDbSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ServiceAccessRoleArn"
                  Core..= serviceAccessRoleArn
              )
          ]
      )
