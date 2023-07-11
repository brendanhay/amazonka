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
-- Module      : Amazonka.SSOAdmin.Types.InstanceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.InstanceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the IAM Identity Center instance.
--
-- /See:/ 'newInstanceMetadata' smart constructor.
data InstanceMetadata = InstanceMetadata'
  { -- | The identifier of the identity store that is connected to the IAM
    -- Identity Center instance.
    identityStoreId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'instanceMetadata_identityStoreId' - The identifier of the identity store that is connected to the IAM
-- Identity Center instance.
--
-- 'instanceArn', 'instanceMetadata_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
newInstanceMetadata ::
  InstanceMetadata
newInstanceMetadata =
  InstanceMetadata'
    { identityStoreId =
        Prelude.Nothing,
      instanceArn = Prelude.Nothing
    }

-- | The identifier of the identity store that is connected to the IAM
-- Identity Center instance.
instanceMetadata_identityStoreId :: Lens.Lens' InstanceMetadata (Prelude.Maybe Prelude.Text)
instanceMetadata_identityStoreId = Lens.lens (\InstanceMetadata' {identityStoreId} -> identityStoreId) (\s@InstanceMetadata' {} a -> s {identityStoreId = a} :: InstanceMetadata)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
instanceMetadata_instanceArn :: Lens.Lens' InstanceMetadata (Prelude.Maybe Prelude.Text)
instanceMetadata_instanceArn = Lens.lens (\InstanceMetadata' {instanceArn} -> instanceArn) (\s@InstanceMetadata' {} a -> s {instanceArn = a} :: InstanceMetadata)

instance Data.FromJSON InstanceMetadata where
  parseJSON =
    Data.withObject
      "InstanceMetadata"
      ( \x ->
          InstanceMetadata'
            Prelude.<$> (x Data..:? "IdentityStoreId")
            Prelude.<*> (x Data..:? "InstanceArn")
      )

instance Prelude.Hashable InstanceMetadata where
  hashWithSalt _salt InstanceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` instanceArn

instance Prelude.NFData InstanceMetadata where
  rnf InstanceMetadata' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf instanceArn
