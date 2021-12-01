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
-- Module      : Amazonka.XRay.Types.ErrorRootCauseService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ErrorRootCauseService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.ErrorRootCauseEntity

-- | A collection of fields identifying the services in a trace summary
-- error.
--
-- /See:/ 'newErrorRootCauseService' smart constructor.
data ErrorRootCauseService = ErrorRootCauseService'
  { -- | The path of root cause entities found on the service.
    entityPath :: Prelude.Maybe [ErrorRootCauseEntity],
    -- | The account ID associated to the service.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | A collection of associated service names.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The service name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating if the service is inferred from the trace.
    inferred :: Prelude.Maybe Prelude.Bool,
    -- | The type associated to the service.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorRootCauseService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityPath', 'errorRootCauseService_entityPath' - The path of root cause entities found on the service.
--
-- 'accountId', 'errorRootCauseService_accountId' - The account ID associated to the service.
--
-- 'names', 'errorRootCauseService_names' - A collection of associated service names.
--
-- 'name', 'errorRootCauseService_name' - The service name.
--
-- 'inferred', 'errorRootCauseService_inferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- 'type'', 'errorRootCauseService_type' - The type associated to the service.
newErrorRootCauseService ::
  ErrorRootCauseService
newErrorRootCauseService =
  ErrorRootCauseService'
    { entityPath =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      names = Prelude.Nothing,
      name = Prelude.Nothing,
      inferred = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The path of root cause entities found on the service.
errorRootCauseService_entityPath :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe [ErrorRootCauseEntity])
errorRootCauseService_entityPath = Lens.lens (\ErrorRootCauseService' {entityPath} -> entityPath) (\s@ErrorRootCauseService' {} a -> s {entityPath = a} :: ErrorRootCauseService) Prelude.. Lens.mapping Lens.coerced

-- | The account ID associated to the service.
errorRootCauseService_accountId :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Text)
errorRootCauseService_accountId = Lens.lens (\ErrorRootCauseService' {accountId} -> accountId) (\s@ErrorRootCauseService' {} a -> s {accountId = a} :: ErrorRootCauseService)

-- | A collection of associated service names.
errorRootCauseService_names :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe [Prelude.Text])
errorRootCauseService_names = Lens.lens (\ErrorRootCauseService' {names} -> names) (\s@ErrorRootCauseService' {} a -> s {names = a} :: ErrorRootCauseService) Prelude.. Lens.mapping Lens.coerced

-- | The service name.
errorRootCauseService_name :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Text)
errorRootCauseService_name = Lens.lens (\ErrorRootCauseService' {name} -> name) (\s@ErrorRootCauseService' {} a -> s {name = a} :: ErrorRootCauseService)

-- | A Boolean value indicating if the service is inferred from the trace.
errorRootCauseService_inferred :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Bool)
errorRootCauseService_inferred = Lens.lens (\ErrorRootCauseService' {inferred} -> inferred) (\s@ErrorRootCauseService' {} a -> s {inferred = a} :: ErrorRootCauseService)

-- | The type associated to the service.
errorRootCauseService_type :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Text)
errorRootCauseService_type = Lens.lens (\ErrorRootCauseService' {type'} -> type') (\s@ErrorRootCauseService' {} a -> s {type' = a} :: ErrorRootCauseService)

instance Core.FromJSON ErrorRootCauseService where
  parseJSON =
    Core.withObject
      "ErrorRootCauseService"
      ( \x ->
          ErrorRootCauseService'
            Prelude.<$> (x Core..:? "EntityPath" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "Names" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Inferred")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable ErrorRootCauseService where
  hashWithSalt salt' ErrorRootCauseService' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` inferred
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` entityPath

instance Prelude.NFData ErrorRootCauseService where
  rnf ErrorRootCauseService' {..} =
    Prelude.rnf entityPath
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf inferred
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf accountId
