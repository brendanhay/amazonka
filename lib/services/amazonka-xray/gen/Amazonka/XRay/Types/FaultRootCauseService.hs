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
-- Module      : Amazonka.XRay.Types.FaultRootCauseService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.FaultRootCauseService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.FaultRootCauseEntity

-- | A collection of fields identifying the services in a trace summary
-- fault.
--
-- /See:/ 'newFaultRootCauseService' smart constructor.
data FaultRootCauseService = FaultRootCauseService'
  { -- | The account ID associated to the service.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The path of root cause entities found on the service.
    entityPath :: Prelude.Maybe [FaultRootCauseEntity],
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
-- Create a value of 'FaultRootCauseService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'faultRootCauseService_accountId' - The account ID associated to the service.
--
-- 'entityPath', 'faultRootCauseService_entityPath' - The path of root cause entities found on the service.
--
-- 'inferred', 'faultRootCauseService_inferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- 'name', 'faultRootCauseService_name' - The service name.
--
-- 'names', 'faultRootCauseService_names' - A collection of associated service names.
--
-- 'type'', 'faultRootCauseService_type' - The type associated to the service.
newFaultRootCauseService ::
  FaultRootCauseService
newFaultRootCauseService =
  FaultRootCauseService'
    { accountId = Prelude.Nothing,
      entityPath = Prelude.Nothing,
      inferred = Prelude.Nothing,
      name = Prelude.Nothing,
      names = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The account ID associated to the service.
faultRootCauseService_accountId :: Lens.Lens' FaultRootCauseService (Prelude.Maybe Prelude.Text)
faultRootCauseService_accountId = Lens.lens (\FaultRootCauseService' {accountId} -> accountId) (\s@FaultRootCauseService' {} a -> s {accountId = a} :: FaultRootCauseService)

-- | The path of root cause entities found on the service.
faultRootCauseService_entityPath :: Lens.Lens' FaultRootCauseService (Prelude.Maybe [FaultRootCauseEntity])
faultRootCauseService_entityPath = Lens.lens (\FaultRootCauseService' {entityPath} -> entityPath) (\s@FaultRootCauseService' {} a -> s {entityPath = a} :: FaultRootCauseService) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value indicating if the service is inferred from the trace.
faultRootCauseService_inferred :: Lens.Lens' FaultRootCauseService (Prelude.Maybe Prelude.Bool)
faultRootCauseService_inferred = Lens.lens (\FaultRootCauseService' {inferred} -> inferred) (\s@FaultRootCauseService' {} a -> s {inferred = a} :: FaultRootCauseService)

-- | The service name.
faultRootCauseService_name :: Lens.Lens' FaultRootCauseService (Prelude.Maybe Prelude.Text)
faultRootCauseService_name = Lens.lens (\FaultRootCauseService' {name} -> name) (\s@FaultRootCauseService' {} a -> s {name = a} :: FaultRootCauseService)

-- | A collection of associated service names.
faultRootCauseService_names :: Lens.Lens' FaultRootCauseService (Prelude.Maybe [Prelude.Text])
faultRootCauseService_names = Lens.lens (\FaultRootCauseService' {names} -> names) (\s@FaultRootCauseService' {} a -> s {names = a} :: FaultRootCauseService) Prelude.. Lens.mapping Lens.coerced

-- | The type associated to the service.
faultRootCauseService_type :: Lens.Lens' FaultRootCauseService (Prelude.Maybe Prelude.Text)
faultRootCauseService_type = Lens.lens (\FaultRootCauseService' {type'} -> type') (\s@FaultRootCauseService' {} a -> s {type' = a} :: FaultRootCauseService)

instance Data.FromJSON FaultRootCauseService where
  parseJSON =
    Data.withObject
      "FaultRootCauseService"
      ( \x ->
          FaultRootCauseService'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "EntityPath" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Inferred")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Names" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable FaultRootCauseService where
  hashWithSalt _salt FaultRootCauseService' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` entityPath
      `Prelude.hashWithSalt` inferred
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FaultRootCauseService where
  rnf FaultRootCauseService' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf entityPath
      `Prelude.seq` Prelude.rnf inferred
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf type'
