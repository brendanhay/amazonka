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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | The component identifier of the resource, generated when DNS target
    -- resource is used.
    componentId :: Prelude.Maybe Prelude.Text,
    -- | The DNS target resource.
    dnsTargetResource :: Prelude.Maybe DNSTargetResource,
    -- | A list of recovery group Amazon Resource Names (ARNs) and cell ARNs that
    -- this resource is contained within.
    readinessScopes :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services resource.
    resourceArn :: Prelude.Maybe Prelude.Text
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
-- 'componentId', 'resource_componentId' - The component identifier of the resource, generated when DNS target
-- resource is used.
--
-- 'dnsTargetResource', 'resource_dnsTargetResource' - The DNS target resource.
--
-- 'readinessScopes', 'resource_readinessScopes' - A list of recovery group Amazon Resource Names (ARNs) and cell ARNs that
-- this resource is contained within.
--
-- 'resourceArn', 'resource_resourceArn' - The Amazon Resource Name (ARN) of the Amazon Web Services resource.
newResource ::
  Resource
newResource =
  Resource'
    { componentId = Prelude.Nothing,
      dnsTargetResource = Prelude.Nothing,
      readinessScopes = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The component identifier of the resource, generated when DNS target
-- resource is used.
resource_componentId :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_componentId = Lens.lens (\Resource' {componentId} -> componentId) (\s@Resource' {} a -> s {componentId = a} :: Resource)

-- | The DNS target resource.
resource_dnsTargetResource :: Lens.Lens' Resource (Prelude.Maybe DNSTargetResource)
resource_dnsTargetResource = Lens.lens (\Resource' {dnsTargetResource} -> dnsTargetResource) (\s@Resource' {} a -> s {dnsTargetResource = a} :: Resource)

-- | A list of recovery group Amazon Resource Names (ARNs) and cell ARNs that
-- this resource is contained within.
resource_readinessScopes :: Lens.Lens' Resource (Prelude.Maybe [Prelude.Text])
resource_readinessScopes = Lens.lens (\Resource' {readinessScopes} -> readinessScopes) (\s@Resource' {} a -> s {readinessScopes = a} :: Resource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Amazon Web Services resource.
resource_resourceArn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceArn = Lens.lens (\Resource' {resourceArn} -> resourceArn) (\s@Resource' {} a -> s {resourceArn = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "componentId")
            Prelude.<*> (x Data..:? "dnsTargetResource")
            Prelude.<*> ( x Data..:? "readinessScopes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "resourceArn")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` dnsTargetResource
      `Prelude.hashWithSalt` readinessScopes
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf dnsTargetResource
      `Prelude.seq` Prelude.rnf readinessScopes
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToJSON Resource where
  toJSON Resource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("componentId" Data..=) Prelude.<$> componentId,
            ("dnsTargetResource" Data..=)
              Prelude.<$> dnsTargetResource,
            ("readinessScopes" Data..=)
              Prelude.<$> readinessScopes,
            ("resourceArn" Data..=) Prelude.<$> resourceArn
          ]
      )
