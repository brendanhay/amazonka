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
-- Module      : Amazonka.AppMesh.Types.EgressFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.EgressFilter where

import Amazonka.AppMesh.Types.EgressFilterType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the egress filter rules for a service mesh.
--
-- /See:/ 'newEgressFilter' smart constructor.
data EgressFilter = EgressFilter'
  { -- | The egress filter type. By default, the type is @DROP_ALL@, which allows
    -- egress only from virtual nodes to other defined resources in the service
    -- mesh (and any traffic to @*.amazonaws.com@ for Amazon Web Services API
    -- calls). You can set the egress filter type to @ALLOW_ALL@ to allow
    -- egress to any endpoint inside or outside of the service mesh.
    type' :: EgressFilterType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EgressFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'egressFilter_type' - The egress filter type. By default, the type is @DROP_ALL@, which allows
-- egress only from virtual nodes to other defined resources in the service
-- mesh (and any traffic to @*.amazonaws.com@ for Amazon Web Services API
-- calls). You can set the egress filter type to @ALLOW_ALL@ to allow
-- egress to any endpoint inside or outside of the service mesh.
newEgressFilter ::
  -- | 'type''
  EgressFilterType ->
  EgressFilter
newEgressFilter pType_ =
  EgressFilter' {type' = pType_}

-- | The egress filter type. By default, the type is @DROP_ALL@, which allows
-- egress only from virtual nodes to other defined resources in the service
-- mesh (and any traffic to @*.amazonaws.com@ for Amazon Web Services API
-- calls). You can set the egress filter type to @ALLOW_ALL@ to allow
-- egress to any endpoint inside or outside of the service mesh.
egressFilter_type :: Lens.Lens' EgressFilter EgressFilterType
egressFilter_type = Lens.lens (\EgressFilter' {type'} -> type') (\s@EgressFilter' {} a -> s {type' = a} :: EgressFilter)

instance Data.FromJSON EgressFilter where
  parseJSON =
    Data.withObject
      "EgressFilter"
      (\x -> EgressFilter' Prelude.<$> (x Data..: "type"))

instance Prelude.Hashable EgressFilter where
  hashWithSalt _salt EgressFilter' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData EgressFilter where
  rnf EgressFilter' {..} = Prelude.rnf type'

instance Data.ToJSON EgressFilter where
  toJSON EgressFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("type" Data..= type')]
      )
