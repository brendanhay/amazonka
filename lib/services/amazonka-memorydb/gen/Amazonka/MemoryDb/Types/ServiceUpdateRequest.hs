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
-- Module      : Amazonka.MemoryDb.Types.ServiceUpdateRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ServiceUpdateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A request to apply a service update
--
-- /See:/ 'newServiceUpdateRequest' smart constructor.
data ServiceUpdateRequest = ServiceUpdateRequest'
  { -- | The unique ID of the service update
    serviceUpdateNameToApply :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceUpdateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceUpdateNameToApply', 'serviceUpdateRequest_serviceUpdateNameToApply' - The unique ID of the service update
newServiceUpdateRequest ::
  ServiceUpdateRequest
newServiceUpdateRequest =
  ServiceUpdateRequest'
    { serviceUpdateNameToApply =
        Prelude.Nothing
    }

-- | The unique ID of the service update
serviceUpdateRequest_serviceUpdateNameToApply :: Lens.Lens' ServiceUpdateRequest (Prelude.Maybe Prelude.Text)
serviceUpdateRequest_serviceUpdateNameToApply = Lens.lens (\ServiceUpdateRequest' {serviceUpdateNameToApply} -> serviceUpdateNameToApply) (\s@ServiceUpdateRequest' {} a -> s {serviceUpdateNameToApply = a} :: ServiceUpdateRequest)

instance Prelude.Hashable ServiceUpdateRequest where
  hashWithSalt _salt ServiceUpdateRequest' {..} =
    _salt
      `Prelude.hashWithSalt` serviceUpdateNameToApply

instance Prelude.NFData ServiceUpdateRequest where
  rnf ServiceUpdateRequest' {..} =
    Prelude.rnf serviceUpdateNameToApply

instance Data.ToJSON ServiceUpdateRequest where
  toJSON ServiceUpdateRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ServiceUpdateNameToApply" Data..=)
              Prelude.<$> serviceUpdateNameToApply
          ]
      )
