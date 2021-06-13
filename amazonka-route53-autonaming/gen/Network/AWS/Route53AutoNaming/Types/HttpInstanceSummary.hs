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
-- Module      : Network.AWS.Route53AutoNaming.Types.HttpInstanceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HttpInstanceSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.HealthStatus

-- | In a response to a
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_DiscoverInstances.html DiscoverInstances>
-- request, @HttpInstanceSummary@ contains information about one instance
-- that matches the values that you specified in the request.
--
-- /See:/ 'newHttpInstanceSummary' smart constructor.
data HttpInstanceSummary = HttpInstanceSummary'
  { -- | The name of the namespace that you specified when you registered the
    -- instance.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The ID of an instance that matches the values that you specified in the
    -- request.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service that you specified when you registered the
    -- instance.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | If you included any attributes when you registered the instance, the
    -- values of those attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | If you configured health checking in the service, the current health
    -- status of the service instance.
    healthStatus :: Prelude.Maybe HealthStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpInstanceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceName', 'httpInstanceSummary_namespaceName' - The name of the namespace that you specified when you registered the
-- instance.
--
-- 'instanceId', 'httpInstanceSummary_instanceId' - The ID of an instance that matches the values that you specified in the
-- request.
--
-- 'serviceName', 'httpInstanceSummary_serviceName' - The name of the service that you specified when you registered the
-- instance.
--
-- 'attributes', 'httpInstanceSummary_attributes' - If you included any attributes when you registered the instance, the
-- values of those attributes.
--
-- 'healthStatus', 'httpInstanceSummary_healthStatus' - If you configured health checking in the service, the current health
-- status of the service instance.
newHttpInstanceSummary ::
  HttpInstanceSummary
newHttpInstanceSummary =
  HttpInstanceSummary'
    { namespaceName =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      attributes = Prelude.Nothing,
      healthStatus = Prelude.Nothing
    }

-- | The name of the namespace that you specified when you registered the
-- instance.
httpInstanceSummary_namespaceName :: Lens.Lens' HttpInstanceSummary (Prelude.Maybe Prelude.Text)
httpInstanceSummary_namespaceName = Lens.lens (\HttpInstanceSummary' {namespaceName} -> namespaceName) (\s@HttpInstanceSummary' {} a -> s {namespaceName = a} :: HttpInstanceSummary)

-- | The ID of an instance that matches the values that you specified in the
-- request.
httpInstanceSummary_instanceId :: Lens.Lens' HttpInstanceSummary (Prelude.Maybe Prelude.Text)
httpInstanceSummary_instanceId = Lens.lens (\HttpInstanceSummary' {instanceId} -> instanceId) (\s@HttpInstanceSummary' {} a -> s {instanceId = a} :: HttpInstanceSummary)

-- | The name of the service that you specified when you registered the
-- instance.
httpInstanceSummary_serviceName :: Lens.Lens' HttpInstanceSummary (Prelude.Maybe Prelude.Text)
httpInstanceSummary_serviceName = Lens.lens (\HttpInstanceSummary' {serviceName} -> serviceName) (\s@HttpInstanceSummary' {} a -> s {serviceName = a} :: HttpInstanceSummary)

-- | If you included any attributes when you registered the instance, the
-- values of those attributes.
httpInstanceSummary_attributes :: Lens.Lens' HttpInstanceSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
httpInstanceSummary_attributes = Lens.lens (\HttpInstanceSummary' {attributes} -> attributes) (\s@HttpInstanceSummary' {} a -> s {attributes = a} :: HttpInstanceSummary) Prelude.. Lens.mapping Lens._Coerce

-- | If you configured health checking in the service, the current health
-- status of the service instance.
httpInstanceSummary_healthStatus :: Lens.Lens' HttpInstanceSummary (Prelude.Maybe HealthStatus)
httpInstanceSummary_healthStatus = Lens.lens (\HttpInstanceSummary' {healthStatus} -> healthStatus) (\s@HttpInstanceSummary' {} a -> s {healthStatus = a} :: HttpInstanceSummary)

instance Core.FromJSON HttpInstanceSummary where
  parseJSON =
    Core.withObject
      "HttpInstanceSummary"
      ( \x ->
          HttpInstanceSummary'
            Prelude.<$> (x Core..:? "NamespaceName")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "ServiceName")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "HealthStatus")
      )

instance Prelude.Hashable HttpInstanceSummary

instance Prelude.NFData HttpInstanceSummary
