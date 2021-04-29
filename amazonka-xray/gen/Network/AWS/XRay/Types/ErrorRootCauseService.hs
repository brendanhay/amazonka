{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.ErrorRootCauseEntity

-- | A collection of fields identifying the services in a trace summary
-- error.
--
-- /See:/ 'newErrorRootCauseService' smart constructor.
data ErrorRootCauseService = ErrorRootCauseService'
  { -- | A collection of associated service names.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The account ID associated to the service.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating if the service is inferred from the trace.
    inferred :: Prelude.Maybe Prelude.Bool,
    -- | The service name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The path of root cause entities found on the service.
    entityPath :: Prelude.Maybe [ErrorRootCauseEntity],
    -- | The type associated to the service.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { names = Prelude.Nothing,
      accountId = Prelude.Nothing,
      inferred = Prelude.Nothing,
      name = Prelude.Nothing,
      entityPath = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A collection of associated service names.
errorRootCauseService_names :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe [Prelude.Text])
errorRootCauseService_names = Lens.lens (\ErrorRootCauseService' {names} -> names) (\s@ErrorRootCauseService' {} a -> s {names = a} :: ErrorRootCauseService) Prelude.. Lens.mapping Prelude._Coerce

-- | The account ID associated to the service.
errorRootCauseService_accountId :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Text)
errorRootCauseService_accountId = Lens.lens (\ErrorRootCauseService' {accountId} -> accountId) (\s@ErrorRootCauseService' {} a -> s {accountId = a} :: ErrorRootCauseService)

-- | A Boolean value indicating if the service is inferred from the trace.
errorRootCauseService_inferred :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Bool)
errorRootCauseService_inferred = Lens.lens (\ErrorRootCauseService' {inferred} -> inferred) (\s@ErrorRootCauseService' {} a -> s {inferred = a} :: ErrorRootCauseService)

-- | The service name.
errorRootCauseService_name :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Text)
errorRootCauseService_name = Lens.lens (\ErrorRootCauseService' {name} -> name) (\s@ErrorRootCauseService' {} a -> s {name = a} :: ErrorRootCauseService)

-- | The path of root cause entities found on the service.
errorRootCauseService_entityPath :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe [ErrorRootCauseEntity])
errorRootCauseService_entityPath = Lens.lens (\ErrorRootCauseService' {entityPath} -> entityPath) (\s@ErrorRootCauseService' {} a -> s {entityPath = a} :: ErrorRootCauseService) Prelude.. Lens.mapping Prelude._Coerce

-- | The type associated to the service.
errorRootCauseService_type :: Lens.Lens' ErrorRootCauseService (Prelude.Maybe Prelude.Text)
errorRootCauseService_type = Lens.lens (\ErrorRootCauseService' {type'} -> type') (\s@ErrorRootCauseService' {} a -> s {type' = a} :: ErrorRootCauseService)

instance Prelude.FromJSON ErrorRootCauseService where
  parseJSON =
    Prelude.withObject
      "ErrorRootCauseService"
      ( \x ->
          ErrorRootCauseService'
            Prelude.<$> (x Prelude..:? "Names" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "AccountId")
            Prelude.<*> (x Prelude..:? "Inferred")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> ( x Prelude..:? "EntityPath"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable ErrorRootCauseService

instance Prelude.NFData ErrorRootCauseService
