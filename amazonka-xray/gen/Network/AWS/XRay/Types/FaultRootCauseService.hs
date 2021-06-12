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
-- Module      : Network.AWS.XRay.Types.FaultRootCauseService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCauseService where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.FaultRootCauseEntity

-- | A collection of fields identifying the services in a trace summary
-- fault.
--
-- /See:/ 'newFaultRootCauseService' smart constructor.
data FaultRootCauseService = FaultRootCauseService'
  { -- | A collection of associated service names.
    names :: Core.Maybe [Core.Text],
    -- | The account ID associated to the service.
    accountId :: Core.Maybe Core.Text,
    -- | A Boolean value indicating if the service is inferred from the trace.
    inferred :: Core.Maybe Core.Bool,
    -- | The service name.
    name :: Core.Maybe Core.Text,
    -- | The path of root cause entities found on the service.
    entityPath :: Core.Maybe [FaultRootCauseEntity],
    -- | The type associated to the service.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FaultRootCauseService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'faultRootCauseService_names' - A collection of associated service names.
--
-- 'accountId', 'faultRootCauseService_accountId' - The account ID associated to the service.
--
-- 'inferred', 'faultRootCauseService_inferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- 'name', 'faultRootCauseService_name' - The service name.
--
-- 'entityPath', 'faultRootCauseService_entityPath' - The path of root cause entities found on the service.
--
-- 'type'', 'faultRootCauseService_type' - The type associated to the service.
newFaultRootCauseService ::
  FaultRootCauseService
newFaultRootCauseService =
  FaultRootCauseService'
    { names = Core.Nothing,
      accountId = Core.Nothing,
      inferred = Core.Nothing,
      name = Core.Nothing,
      entityPath = Core.Nothing,
      type' = Core.Nothing
    }

-- | A collection of associated service names.
faultRootCauseService_names :: Lens.Lens' FaultRootCauseService (Core.Maybe [Core.Text])
faultRootCauseService_names = Lens.lens (\FaultRootCauseService' {names} -> names) (\s@FaultRootCauseService' {} a -> s {names = a} :: FaultRootCauseService) Core.. Lens.mapping Lens._Coerce

-- | The account ID associated to the service.
faultRootCauseService_accountId :: Lens.Lens' FaultRootCauseService (Core.Maybe Core.Text)
faultRootCauseService_accountId = Lens.lens (\FaultRootCauseService' {accountId} -> accountId) (\s@FaultRootCauseService' {} a -> s {accountId = a} :: FaultRootCauseService)

-- | A Boolean value indicating if the service is inferred from the trace.
faultRootCauseService_inferred :: Lens.Lens' FaultRootCauseService (Core.Maybe Core.Bool)
faultRootCauseService_inferred = Lens.lens (\FaultRootCauseService' {inferred} -> inferred) (\s@FaultRootCauseService' {} a -> s {inferred = a} :: FaultRootCauseService)

-- | The service name.
faultRootCauseService_name :: Lens.Lens' FaultRootCauseService (Core.Maybe Core.Text)
faultRootCauseService_name = Lens.lens (\FaultRootCauseService' {name} -> name) (\s@FaultRootCauseService' {} a -> s {name = a} :: FaultRootCauseService)

-- | The path of root cause entities found on the service.
faultRootCauseService_entityPath :: Lens.Lens' FaultRootCauseService (Core.Maybe [FaultRootCauseEntity])
faultRootCauseService_entityPath = Lens.lens (\FaultRootCauseService' {entityPath} -> entityPath) (\s@FaultRootCauseService' {} a -> s {entityPath = a} :: FaultRootCauseService) Core.. Lens.mapping Lens._Coerce

-- | The type associated to the service.
faultRootCauseService_type :: Lens.Lens' FaultRootCauseService (Core.Maybe Core.Text)
faultRootCauseService_type = Lens.lens (\FaultRootCauseService' {type'} -> type') (\s@FaultRootCauseService' {} a -> s {type' = a} :: FaultRootCauseService)

instance Core.FromJSON FaultRootCauseService where
  parseJSON =
    Core.withObject
      "FaultRootCauseService"
      ( \x ->
          FaultRootCauseService'
            Core.<$> (x Core..:? "Names" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AccountId")
            Core.<*> (x Core..:? "Inferred")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "EntityPath" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable FaultRootCauseService

instance Core.NFData FaultRootCauseService
