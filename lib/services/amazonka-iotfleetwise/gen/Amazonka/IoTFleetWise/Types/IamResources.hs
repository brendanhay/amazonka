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
-- Module      : Amazonka.IoTFleetWise.Types.IamResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.IamResources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The IAM resource that enables Amazon Web Services IoT FleetWise edge
-- agent software to send data to Amazon Timestream.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM roles>
-- in the /Identity and Access Management User Guide/.
--
-- /See:/ 'newIamResources' smart constructor.
data IamResources = IamResources'
  { -- | The Amazon Resource Name (ARN) of the IAM resource that allows Amazon
    -- Web Services IoT FleetWise to send data to Amazon Timestream. For
    -- example, @arn:aws:iam::123456789012:role\/SERVICE-ROLE-ARN@.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IamResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'iamResources_roleArn' - The Amazon Resource Name (ARN) of the IAM resource that allows Amazon
-- Web Services IoT FleetWise to send data to Amazon Timestream. For
-- example, @arn:aws:iam::123456789012:role\/SERVICE-ROLE-ARN@.
newIamResources ::
  -- | 'roleArn'
  Prelude.Text ->
  IamResources
newIamResources pRoleArn_ =
  IamResources' {roleArn = pRoleArn_}

-- | The Amazon Resource Name (ARN) of the IAM resource that allows Amazon
-- Web Services IoT FleetWise to send data to Amazon Timestream. For
-- example, @arn:aws:iam::123456789012:role\/SERVICE-ROLE-ARN@.
iamResources_roleArn :: Lens.Lens' IamResources Prelude.Text
iamResources_roleArn = Lens.lens (\IamResources' {roleArn} -> roleArn) (\s@IamResources' {} a -> s {roleArn = a} :: IamResources)

instance Data.FromJSON IamResources where
  parseJSON =
    Data.withObject
      "IamResources"
      ( \x ->
          IamResources' Prelude.<$> (x Data..: "roleArn")
      )

instance Prelude.Hashable IamResources where
  hashWithSalt _salt IamResources' {..} =
    _salt `Prelude.hashWithSalt` roleArn

instance Prelude.NFData IamResources where
  rnf IamResources' {..} = Prelude.rnf roleArn

instance Data.ToJSON IamResources where
  toJSON IamResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("roleArn" Data..= roleArn)]
      )
