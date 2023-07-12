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
-- Module      : Amazonka.Route53.Types.LinkedService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.LinkedService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | If a health check or hosted zone was created by another service,
-- @LinkedService@ is a complex type that describes the service that
-- created the resource. When a resource is created by another service, you
-- can\'t edit or delete it using Amazon Route 53.
--
-- /See:/ 'newLinkedService' smart constructor.
data LinkedService = LinkedService'
  { -- | If the health check or hosted zone was created by another service, an
    -- optional description that can be provided by the other service. When a
    -- resource is created by another service, you can\'t edit or delete it
    -- using Amazon Route 53.
    description :: Prelude.Maybe Prelude.Text,
    -- | If the health check or hosted zone was created by another service, the
    -- service that created the resource. When a resource is created by another
    -- service, you can\'t edit or delete it using Amazon Route 53.
    servicePrincipal :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LinkedService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'linkedService_description' - If the health check or hosted zone was created by another service, an
-- optional description that can be provided by the other service. When a
-- resource is created by another service, you can\'t edit or delete it
-- using Amazon Route 53.
--
-- 'servicePrincipal', 'linkedService_servicePrincipal' - If the health check or hosted zone was created by another service, the
-- service that created the resource. When a resource is created by another
-- service, you can\'t edit or delete it using Amazon Route 53.
newLinkedService ::
  LinkedService
newLinkedService =
  LinkedService'
    { description = Prelude.Nothing,
      servicePrincipal = Prelude.Nothing
    }

-- | If the health check or hosted zone was created by another service, an
-- optional description that can be provided by the other service. When a
-- resource is created by another service, you can\'t edit or delete it
-- using Amazon Route 53.
linkedService_description :: Lens.Lens' LinkedService (Prelude.Maybe Prelude.Text)
linkedService_description = Lens.lens (\LinkedService' {description} -> description) (\s@LinkedService' {} a -> s {description = a} :: LinkedService)

-- | If the health check or hosted zone was created by another service, the
-- service that created the resource. When a resource is created by another
-- service, you can\'t edit or delete it using Amazon Route 53.
linkedService_servicePrincipal :: Lens.Lens' LinkedService (Prelude.Maybe Prelude.Text)
linkedService_servicePrincipal = Lens.lens (\LinkedService' {servicePrincipal} -> servicePrincipal) (\s@LinkedService' {} a -> s {servicePrincipal = a} :: LinkedService)

instance Data.FromXML LinkedService where
  parseXML x =
    LinkedService'
      Prelude.<$> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "ServicePrincipal")

instance Prelude.Hashable LinkedService where
  hashWithSalt _salt LinkedService' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` servicePrincipal

instance Prelude.NFData LinkedService where
  rnf LinkedService' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf servicePrincipal
