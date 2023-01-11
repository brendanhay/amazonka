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
-- Module      : Amazonka.EC2.Types.InstanceRequirementsWithMetadataRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceRequirementsWithMetadataRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ArchitectureType
import Amazonka.EC2.Types.InstanceRequirementsRequest
import Amazonka.EC2.Types.VirtualizationType
import qualified Amazonka.Prelude as Prelude

-- | The architecture type, virtualization type, and other attributes for the
-- instance types. When you specify instance attributes, Amazon EC2 will
-- identify instance types with those attributes.
--
-- If you specify @InstanceRequirementsWithMetadataRequest@, you can\'t
-- specify @InstanceTypes@.
--
-- /See:/ 'newInstanceRequirementsWithMetadataRequest' smart constructor.
data InstanceRequirementsWithMetadataRequest = InstanceRequirementsWithMetadataRequest'
  { -- | The architecture type.
    architectureTypes :: Prelude.Maybe [ArchitectureType],
    -- | The attributes for the instance types. When you specify instance
    -- attributes, Amazon EC2 will identify instance types with those
    -- attributes.
    instanceRequirements :: Prelude.Maybe InstanceRequirementsRequest,
    -- | The virtualization type.
    virtualizationTypes :: Prelude.Maybe [VirtualizationType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceRequirementsWithMetadataRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architectureTypes', 'instanceRequirementsWithMetadataRequest_architectureTypes' - The architecture type.
--
-- 'instanceRequirements', 'instanceRequirementsWithMetadataRequest_instanceRequirements' - The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
--
-- 'virtualizationTypes', 'instanceRequirementsWithMetadataRequest_virtualizationTypes' - The virtualization type.
newInstanceRequirementsWithMetadataRequest ::
  InstanceRequirementsWithMetadataRequest
newInstanceRequirementsWithMetadataRequest =
  InstanceRequirementsWithMetadataRequest'
    { architectureTypes =
        Prelude.Nothing,
      instanceRequirements =
        Prelude.Nothing,
      virtualizationTypes =
        Prelude.Nothing
    }

-- | The architecture type.
instanceRequirementsWithMetadataRequest_architectureTypes :: Lens.Lens' InstanceRequirementsWithMetadataRequest (Prelude.Maybe [ArchitectureType])
instanceRequirementsWithMetadataRequest_architectureTypes = Lens.lens (\InstanceRequirementsWithMetadataRequest' {architectureTypes} -> architectureTypes) (\s@InstanceRequirementsWithMetadataRequest' {} a -> s {architectureTypes = a} :: InstanceRequirementsWithMetadataRequest) Prelude.. Lens.mapping Lens.coerced

-- | The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
instanceRequirementsWithMetadataRequest_instanceRequirements :: Lens.Lens' InstanceRequirementsWithMetadataRequest (Prelude.Maybe InstanceRequirementsRequest)
instanceRequirementsWithMetadataRequest_instanceRequirements = Lens.lens (\InstanceRequirementsWithMetadataRequest' {instanceRequirements} -> instanceRequirements) (\s@InstanceRequirementsWithMetadataRequest' {} a -> s {instanceRequirements = a} :: InstanceRequirementsWithMetadataRequest)

-- | The virtualization type.
instanceRequirementsWithMetadataRequest_virtualizationTypes :: Lens.Lens' InstanceRequirementsWithMetadataRequest (Prelude.Maybe [VirtualizationType])
instanceRequirementsWithMetadataRequest_virtualizationTypes = Lens.lens (\InstanceRequirementsWithMetadataRequest' {virtualizationTypes} -> virtualizationTypes) (\s@InstanceRequirementsWithMetadataRequest' {} a -> s {virtualizationTypes = a} :: InstanceRequirementsWithMetadataRequest) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    InstanceRequirementsWithMetadataRequest
  where
  hashWithSalt
    _salt
    InstanceRequirementsWithMetadataRequest' {..} =
      _salt `Prelude.hashWithSalt` architectureTypes
        `Prelude.hashWithSalt` instanceRequirements
        `Prelude.hashWithSalt` virtualizationTypes

instance
  Prelude.NFData
    InstanceRequirementsWithMetadataRequest
  where
  rnf InstanceRequirementsWithMetadataRequest' {..} =
    Prelude.rnf architectureTypes
      `Prelude.seq` Prelude.rnf instanceRequirements
      `Prelude.seq` Prelude.rnf virtualizationTypes

instance
  Data.ToQuery
    InstanceRequirementsWithMetadataRequest
  where
  toQuery InstanceRequirementsWithMetadataRequest' {..} =
    Prelude.mconcat
      [ Data.toQuery
          ( Data.toQueryList "ArchitectureType"
              Prelude.<$> architectureTypes
          ),
        "InstanceRequirements" Data.=: instanceRequirements,
        Data.toQuery
          ( Data.toQueryList "VirtualizationType"
              Prelude.<$> virtualizationTypes
          )
      ]
