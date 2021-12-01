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
-- Module      : Amazonka.XRay.Types.ResponseTimeRootCauseService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ResponseTimeRootCauseService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.ResponseTimeRootCauseEntity

-- | A collection of fields identifying the service in a response time
-- warning.
--
-- /See:/ 'newResponseTimeRootCauseService' smart constructor.
data ResponseTimeRootCauseService = ResponseTimeRootCauseService'
  { -- | The path of root cause entities found on the service.
    entityPath :: Prelude.Maybe [ResponseTimeRootCauseEntity],
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
-- Create a value of 'ResponseTimeRootCauseService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityPath', 'responseTimeRootCauseService_entityPath' - The path of root cause entities found on the service.
--
-- 'accountId', 'responseTimeRootCauseService_accountId' - The account ID associated to the service.
--
-- 'names', 'responseTimeRootCauseService_names' - A collection of associated service names.
--
-- 'name', 'responseTimeRootCauseService_name' - The service name.
--
-- 'inferred', 'responseTimeRootCauseService_inferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- 'type'', 'responseTimeRootCauseService_type' - The type associated to the service.
newResponseTimeRootCauseService ::
  ResponseTimeRootCauseService
newResponseTimeRootCauseService =
  ResponseTimeRootCauseService'
    { entityPath =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      names = Prelude.Nothing,
      name = Prelude.Nothing,
      inferred = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The path of root cause entities found on the service.
responseTimeRootCauseService_entityPath :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe [ResponseTimeRootCauseEntity])
responseTimeRootCauseService_entityPath = Lens.lens (\ResponseTimeRootCauseService' {entityPath} -> entityPath) (\s@ResponseTimeRootCauseService' {} a -> s {entityPath = a} :: ResponseTimeRootCauseService) Prelude.. Lens.mapping Lens.coerced

-- | The account ID associated to the service.
responseTimeRootCauseService_accountId :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe Prelude.Text)
responseTimeRootCauseService_accountId = Lens.lens (\ResponseTimeRootCauseService' {accountId} -> accountId) (\s@ResponseTimeRootCauseService' {} a -> s {accountId = a} :: ResponseTimeRootCauseService)

-- | A collection of associated service names.
responseTimeRootCauseService_names :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe [Prelude.Text])
responseTimeRootCauseService_names = Lens.lens (\ResponseTimeRootCauseService' {names} -> names) (\s@ResponseTimeRootCauseService' {} a -> s {names = a} :: ResponseTimeRootCauseService) Prelude.. Lens.mapping Lens.coerced

-- | The service name.
responseTimeRootCauseService_name :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe Prelude.Text)
responseTimeRootCauseService_name = Lens.lens (\ResponseTimeRootCauseService' {name} -> name) (\s@ResponseTimeRootCauseService' {} a -> s {name = a} :: ResponseTimeRootCauseService)

-- | A Boolean value indicating if the service is inferred from the trace.
responseTimeRootCauseService_inferred :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe Prelude.Bool)
responseTimeRootCauseService_inferred = Lens.lens (\ResponseTimeRootCauseService' {inferred} -> inferred) (\s@ResponseTimeRootCauseService' {} a -> s {inferred = a} :: ResponseTimeRootCauseService)

-- | The type associated to the service.
responseTimeRootCauseService_type :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe Prelude.Text)
responseTimeRootCauseService_type = Lens.lens (\ResponseTimeRootCauseService' {type'} -> type') (\s@ResponseTimeRootCauseService' {} a -> s {type' = a} :: ResponseTimeRootCauseService)

instance Core.FromJSON ResponseTimeRootCauseService where
  parseJSON =
    Core.withObject
      "ResponseTimeRootCauseService"
      ( \x ->
          ResponseTimeRootCauseService'
            Prelude.<$> (x Core..:? "EntityPath" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "Names" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Inferred")
            Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    ResponseTimeRootCauseService
  where
  hashWithSalt salt' ResponseTimeRootCauseService' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` inferred
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` entityPath

instance Prelude.NFData ResponseTimeRootCauseService where
  rnf ResponseTimeRootCauseService' {..} =
    Prelude.rnf entityPath
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf inferred
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf accountId
