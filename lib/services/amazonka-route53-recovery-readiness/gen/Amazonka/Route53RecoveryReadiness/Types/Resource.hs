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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.Resource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryReadiness.Types.DNSTargetResource

-- | The resource element of a resource set.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | A list of recovery group Amazon Resource Names (ARNs) and cell ARNs that
    -- this resource is contained within.
    readinessScopes :: Prelude.Maybe [Prelude.Text],
    -- | The DNS target resource.
    dnsTargetResource :: Prelude.Maybe DNSTargetResource,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The component identifier of the resource, generated when DNS target
    -- resource is used.
    componentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readinessScopes', 'resource_readinessScopes' - A list of recovery group Amazon Resource Names (ARNs) and cell ARNs that
-- this resource is contained within.
--
-- 'dnsTargetResource', 'resource_dnsTargetResource' - The DNS target resource.
--
-- 'resourceArn', 'resource_resourceArn' - The Amazon Resource Name (ARN) of the Amazon Web Services resource.
--
-- 'componentId', 'resource_componentId' - The component identifier of the resource, generated when DNS target
-- resource is used.
newResource ::
  Resource
newResource =
  Resource'
    { readinessScopes = Prelude.Nothing,
      dnsTargetResource = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      componentId = Prelude.Nothing
    }

-- | A list of recovery group Amazon Resource Names (ARNs) and cell ARNs that
-- this resource is contained within.
resource_readinessScopes :: Lens.Lens' Resource (Prelude.Maybe [Prelude.Text])
resource_readinessScopes = Lens.lens (\Resource' {readinessScopes} -> readinessScopes) (\s@Resource' {} a -> s {readinessScopes = a} :: Resource) Prelude.. Lens.mapping Lens.coerced

-- | The DNS target resource.
resource_dnsTargetResource :: Lens.Lens' Resource (Prelude.Maybe DNSTargetResource)
resource_dnsTargetResource = Lens.lens (\Resource' {dnsTargetResource} -> dnsTargetResource) (\s@Resource' {} a -> s {dnsTargetResource = a} :: Resource)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services resource.
resource_resourceArn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceArn = Lens.lens (\Resource' {resourceArn} -> resourceArn) (\s@Resource' {} a -> s {resourceArn = a} :: Resource)

-- | The component identifier of the resource, generated when DNS target
-- resource is used.
resource_componentId :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_componentId = Lens.lens (\Resource' {componentId} -> componentId) (\s@Resource' {} a -> s {componentId = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> ( x Data..:? "readinessScopes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "dnsTargetResource")
            Prelude.<*> (x Data..:? "resourceArn")
            Prelude.<*> (x Data..:? "componentId")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` readinessScopes
      `Prelude.hashWithSalt` dnsTargetResource
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` componentId

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf readinessScopes
      `Prelude.seq` Prelude.rnf dnsTargetResource
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf componentId

instance Data.ToJSON Resource where
  toJSON Resource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("readinessScopes" Data..=)
              Prelude.<$> readinessScopes,
            ("dnsTargetResource" Data..=)
              Prelude.<$> dnsTargetResource,
            ("resourceArn" Data..=) Prelude.<$> resourceArn,
            ("componentId" Data..=) Prelude.<$> componentId
          ]
      )
