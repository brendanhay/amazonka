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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ErrorRootCauseService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.ErrorRootCauseEntity

-- | A collection of fields identifying the services in a trace summary
-- error.
--
-- /See:/ 'newErrorRootCauseService' smart constructor.
data ErrorRootCauseService = ErrorRootCauseService'
  { -- | The account ID associated to the service.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The path of root cause entities found on the service.
    entityPath :: Prelude.Maybe [ErrorRootCauseEntity],
    -- | A Boolean value indicating if the service is inferred from the trace.
    inferred :: Prelude.Maybe Prelude.Bool,
    -- | The service name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A collection of associated service names.
    names :: Prelude.Maybe [Prelude.Text],
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
-- 'accountId', 'errorRootCauseService_accountId' - The account ID associated to the service.
--
-- 'entityPath', 'errorRootCauseService_entityPath' - The path of root cause entities found on the service.
--
-- 'inferred', 'errorRootCauseService_inferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- 'name', 'errorRootCauseService_name' - The service name.
--
-- 'names', 'errorRootCauseService_names' - A collection of associated service names.
--
-- 'type'', 'errorRootCauseService_type' - The type associated to the service.
newErrorRootCauseService ::
  ErrorRootCauseService
newErrorRootCauseService =
  ErrorRootCauseService'
    { accountId = Prelude.Nothing,
      entityPath = Prelude.Nothing,
      inferred = Prelude.Nothing,
      name = Prelude.Nothing,
      names = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The account ID associated to the service.
errorRootCauseService_accountId :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Text)
errorRootCauseService_accountId = Lens.lens (\ErrorRootCauseService' {accountId} -> accountId) (\s@ErrorRootCauseService' {} a -> s {accountId = a} :: ErrorRootCauseService)

-- | The path of root cause entities found on the service.
errorRootCauseService_entityPath :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe [ErrorRootCauseEntity])
errorRootCauseService_entityPath = Lens.lens (\ErrorRootCauseService' {entityPath} -> entityPath) (\s@ErrorRootCauseService' {} a -> s {entityPath = a} :: ErrorRootCauseService) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value indicating if the service is inferred from the trace.
errorRootCauseService_inferred :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Bool)
errorRootCauseService_inferred = Lens.lens (\ErrorRootCauseService' {inferred} -> inferred) (\s@ErrorRootCauseService' {} a -> s {inferred = a} :: ErrorRootCauseService)

-- | The service name.
errorRootCauseService_name :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Text)
errorRootCauseService_name = Lens.lens (\ErrorRootCauseService' {name} -> name) (\s@ErrorRootCauseService' {} a -> s {name = a} :: ErrorRootCauseService)

-- | A collection of associated service names.
errorRootCauseService_names :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe [Prelude.Text])
errorRootCauseService_names = Lens.lens (\ErrorRootCauseService' {names} -> names) (\s@ErrorRootCauseService' {} a -> s {names = a} :: ErrorRootCauseService) Prelude.. Lens.mapping Lens.coerced

-- | The type associated to the service.
errorRootCauseService_type :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Text)
errorRootCauseService_type = Lens.lens (\ErrorRootCauseService' {type'} -> type') (\s@ErrorRootCauseService' {} a -> s {type' = a} :: ErrorRootCauseService)

instance Data.FromJSON ErrorRootCauseService where
  parseJSON =
    Data.withObject
      "ErrorRootCauseService"
      ( \x ->
          ErrorRootCauseService'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "EntityPath" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Inferred")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Names" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ErrorRootCauseService where
  hashWithSalt _salt ErrorRootCauseService' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` entityPath
      `Prelude.hashWithSalt` inferred
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ErrorRootCauseService where
  rnf ErrorRootCauseService' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf entityPath
      `Prelude.seq` Prelude.rnf inferred
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf type'
