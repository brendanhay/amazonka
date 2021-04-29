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
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCauseService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCauseService where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.ResponseTimeRootCauseEntity

-- | A collection of fields identifying the service in a response time
-- warning.
--
-- /See:/ 'newResponseTimeRootCauseService' smart constructor.
data ResponseTimeRootCauseService = ResponseTimeRootCauseService'
  { -- | A collection of associated service names.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The account ID associated to the service.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating if the service is inferred from the trace.
    inferred :: Prelude.Maybe Prelude.Bool,
    -- | The service name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The path of root cause entities found on the service.
    entityPath :: Prelude.Maybe [ResponseTimeRootCauseEntity],
    -- | The type associated to the service.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResponseTimeRootCauseService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'responseTimeRootCauseService_names' - A collection of associated service names.
--
-- 'accountId', 'responseTimeRootCauseService_accountId' - The account ID associated to the service.
--
-- 'inferred', 'responseTimeRootCauseService_inferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- 'name', 'responseTimeRootCauseService_name' - The service name.
--
-- 'entityPath', 'responseTimeRootCauseService_entityPath' - The path of root cause entities found on the service.
--
-- 'type'', 'responseTimeRootCauseService_type' - The type associated to the service.
newResponseTimeRootCauseService ::
  ResponseTimeRootCauseService
newResponseTimeRootCauseService =
  ResponseTimeRootCauseService'
    { names =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      inferred = Prelude.Nothing,
      name = Prelude.Nothing,
      entityPath = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A collection of associated service names.
responseTimeRootCauseService_names :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe [Prelude.Text])
responseTimeRootCauseService_names = Lens.lens (\ResponseTimeRootCauseService' {names} -> names) (\s@ResponseTimeRootCauseService' {} a -> s {names = a} :: ResponseTimeRootCauseService) Prelude.. Lens.mapping Prelude._Coerce

-- | The account ID associated to the service.
responseTimeRootCauseService_accountId :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe Prelude.Text)
responseTimeRootCauseService_accountId = Lens.lens (\ResponseTimeRootCauseService' {accountId} -> accountId) (\s@ResponseTimeRootCauseService' {} a -> s {accountId = a} :: ResponseTimeRootCauseService)

-- | A Boolean value indicating if the service is inferred from the trace.
responseTimeRootCauseService_inferred :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe Prelude.Bool)
responseTimeRootCauseService_inferred = Lens.lens (\ResponseTimeRootCauseService' {inferred} -> inferred) (\s@ResponseTimeRootCauseService' {} a -> s {inferred = a} :: ResponseTimeRootCauseService)

-- | The service name.
responseTimeRootCauseService_name :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe Prelude.Text)
responseTimeRootCauseService_name = Lens.lens (\ResponseTimeRootCauseService' {name} -> name) (\s@ResponseTimeRootCauseService' {} a -> s {name = a} :: ResponseTimeRootCauseService)

-- | The path of root cause entities found on the service.
responseTimeRootCauseService_entityPath :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe [ResponseTimeRootCauseEntity])
responseTimeRootCauseService_entityPath = Lens.lens (\ResponseTimeRootCauseService' {entityPath} -> entityPath) (\s@ResponseTimeRootCauseService' {} a -> s {entityPath = a} :: ResponseTimeRootCauseService) Prelude.. Lens.mapping Prelude._Coerce

-- | The type associated to the service.
responseTimeRootCauseService_type :: Lens.Lens' ResponseTimeRootCauseService (Prelude.Maybe Prelude.Text)
responseTimeRootCauseService_type = Lens.lens (\ResponseTimeRootCauseService' {type'} -> type') (\s@ResponseTimeRootCauseService' {} a -> s {type' = a} :: ResponseTimeRootCauseService)

instance
  Prelude.FromJSON
    ResponseTimeRootCauseService
  where
  parseJSON =
    Prelude.withObject
      "ResponseTimeRootCauseService"
      ( \x ->
          ResponseTimeRootCauseService'
            Prelude.<$> (x Prelude..:? "Names" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "AccountId")
            Prelude.<*> (x Prelude..:? "Inferred")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> ( x Prelude..:? "EntityPath"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Type")
      )

instance
  Prelude.Hashable
    ResponseTimeRootCauseService

instance Prelude.NFData ResponseTimeRootCauseService
