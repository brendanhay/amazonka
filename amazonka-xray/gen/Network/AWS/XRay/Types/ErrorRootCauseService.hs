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
-- Module      : Network.AWS.XRay.Types.ErrorRootCauseService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCauseService where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.ErrorRootCauseEntity

-- | A collection of fields identifying the services in a trace summary
-- error.
--
-- /See:/ 'newErrorRootCauseService' smart constructor.
data ErrorRootCauseService = ErrorRootCauseService'
  { -- | A collection of associated service names.
    names :: Core.Maybe [Core.Text],
    -- | The account ID associated to the service.
    accountId :: Core.Maybe Core.Text,
    -- | A Boolean value indicating if the service is inferred from the trace.
    inferred :: Core.Maybe Core.Bool,
    -- | The service name.
    name :: Core.Maybe Core.Text,
    -- | The path of root cause entities found on the service.
    entityPath :: Core.Maybe [ErrorRootCauseEntity],
    -- | The type associated to the service.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ErrorRootCauseService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'errorRootCauseService_names' - A collection of associated service names.
--
-- 'accountId', 'errorRootCauseService_accountId' - The account ID associated to the service.
--
-- 'inferred', 'errorRootCauseService_inferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- 'name', 'errorRootCauseService_name' - The service name.
--
-- 'entityPath', 'errorRootCauseService_entityPath' - The path of root cause entities found on the service.
--
-- 'type'', 'errorRootCauseService_type' - The type associated to the service.
newErrorRootCauseService ::
  ErrorRootCauseService
newErrorRootCauseService =
  ErrorRootCauseService'
    { names = Core.Nothing,
      accountId = Core.Nothing,
      inferred = Core.Nothing,
      name = Core.Nothing,
      entityPath = Core.Nothing,
      type' = Core.Nothing
    }

-- | A collection of associated service names.
errorRootCauseService_names :: Lens.Lens' ErrorRootCauseService (Core.Maybe [Core.Text])
errorRootCauseService_names = Lens.lens (\ErrorRootCauseService' {names} -> names) (\s@ErrorRootCauseService' {} a -> s {names = a} :: ErrorRootCauseService) Core.. Lens.mapping Lens._Coerce

-- | The account ID associated to the service.
errorRootCauseService_accountId :: Lens.Lens' ErrorRootCauseService (Core.Maybe Core.Text)
errorRootCauseService_accountId = Lens.lens (\ErrorRootCauseService' {accountId} -> accountId) (\s@ErrorRootCauseService' {} a -> s {accountId = a} :: ErrorRootCauseService)

-- | A Boolean value indicating if the service is inferred from the trace.
errorRootCauseService_inferred :: Lens.Lens' ErrorRootCauseService (Core.Maybe Core.Bool)
errorRootCauseService_inferred = Lens.lens (\ErrorRootCauseService' {inferred} -> inferred) (\s@ErrorRootCauseService' {} a -> s {inferred = a} :: ErrorRootCauseService)

-- | The service name.
errorRootCauseService_name :: Lens.Lens' ErrorRootCauseService (Core.Maybe Core.Text)
errorRootCauseService_name = Lens.lens (\ErrorRootCauseService' {name} -> name) (\s@ErrorRootCauseService' {} a -> s {name = a} :: ErrorRootCauseService)

-- | The path of root cause entities found on the service.
errorRootCauseService_entityPath :: Lens.Lens' ErrorRootCauseService (Core.Maybe [ErrorRootCauseEntity])
errorRootCauseService_entityPath = Lens.lens (\ErrorRootCauseService' {entityPath} -> entityPath) (\s@ErrorRootCauseService' {} a -> s {entityPath = a} :: ErrorRootCauseService) Core.. Lens.mapping Lens._Coerce

-- | The type associated to the service.
errorRootCauseService_type :: Lens.Lens' ErrorRootCauseService (Core.Maybe Core.Text)
errorRootCauseService_type = Lens.lens (\ErrorRootCauseService' {type'} -> type') (\s@ErrorRootCauseService' {} a -> s {type' = a} :: ErrorRootCauseService)

instance Core.FromJSON ErrorRootCauseService where
  parseJSON =
    Core.withObject
      "ErrorRootCauseService"
      ( \x ->
          ErrorRootCauseService'
            Core.<$> (x Core..:? "Names" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AccountId")
            Core.<*> (x Core..:? "Inferred")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "EntityPath" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable ErrorRootCauseService

instance Core.NFData ErrorRootCauseService
