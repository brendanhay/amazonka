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
-- Module      : Amazonka.Shield.Types.Protection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.Protection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.ApplicationLayerAutomaticResponseConfiguration

-- | An object that represents a resource that is under DDoS protection.
--
-- /See:/ 'newProtection' smart constructor.
data Protection = Protection'
  { -- | The automatic application layer DDoS mitigation settings for the
    -- protection. This configuration determines whether Shield Advanced
    -- automatically manages rules in the web ACL in order to respond to
    -- application layer events that Shield Advanced determines to be DDoS
    -- attacks.
    applicationLayerAutomaticResponseConfiguration :: Prelude.Maybe ApplicationLayerAutomaticResponseConfiguration,
    -- | The unique identifier (ID) for the Route 53 health check that\'s
    -- associated with the protection.
    healthCheckIds :: Prelude.Maybe [Prelude.Text],
    -- | The unique identifier (ID) of the protection.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the protection. For example, @My CloudFront distributions@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the protection.
    protectionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the Amazon Web Services resource that
    -- is protected.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Protection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationLayerAutomaticResponseConfiguration', 'protection_applicationLayerAutomaticResponseConfiguration' - The automatic application layer DDoS mitigation settings for the
-- protection. This configuration determines whether Shield Advanced
-- automatically manages rules in the web ACL in order to respond to
-- application layer events that Shield Advanced determines to be DDoS
-- attacks.
--
-- 'healthCheckIds', 'protection_healthCheckIds' - The unique identifier (ID) for the Route 53 health check that\'s
-- associated with the protection.
--
-- 'id', 'protection_id' - The unique identifier (ID) of the protection.
--
-- 'name', 'protection_name' - The name of the protection. For example, @My CloudFront distributions@.
--
-- 'protectionArn', 'protection_protectionArn' - The ARN (Amazon Resource Name) of the protection.
--
-- 'resourceArn', 'protection_resourceArn' - The ARN (Amazon Resource Name) of the Amazon Web Services resource that
-- is protected.
newProtection ::
  Protection
newProtection =
  Protection'
    { applicationLayerAutomaticResponseConfiguration =
        Prelude.Nothing,
      healthCheckIds = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      protectionArn = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The automatic application layer DDoS mitigation settings for the
-- protection. This configuration determines whether Shield Advanced
-- automatically manages rules in the web ACL in order to respond to
-- application layer events that Shield Advanced determines to be DDoS
-- attacks.
protection_applicationLayerAutomaticResponseConfiguration :: Lens.Lens' Protection (Prelude.Maybe ApplicationLayerAutomaticResponseConfiguration)
protection_applicationLayerAutomaticResponseConfiguration = Lens.lens (\Protection' {applicationLayerAutomaticResponseConfiguration} -> applicationLayerAutomaticResponseConfiguration) (\s@Protection' {} a -> s {applicationLayerAutomaticResponseConfiguration = a} :: Protection)

-- | The unique identifier (ID) for the Route 53 health check that\'s
-- associated with the protection.
protection_healthCheckIds :: Lens.Lens' Protection (Prelude.Maybe [Prelude.Text])
protection_healthCheckIds = Lens.lens (\Protection' {healthCheckIds} -> healthCheckIds) (\s@Protection' {} a -> s {healthCheckIds = a} :: Protection) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier (ID) of the protection.
protection_id :: Lens.Lens' Protection (Prelude.Maybe Prelude.Text)
protection_id = Lens.lens (\Protection' {id} -> id) (\s@Protection' {} a -> s {id = a} :: Protection)

-- | The name of the protection. For example, @My CloudFront distributions@.
protection_name :: Lens.Lens' Protection (Prelude.Maybe Prelude.Text)
protection_name = Lens.lens (\Protection' {name} -> name) (\s@Protection' {} a -> s {name = a} :: Protection)

-- | The ARN (Amazon Resource Name) of the protection.
protection_protectionArn :: Lens.Lens' Protection (Prelude.Maybe Prelude.Text)
protection_protectionArn = Lens.lens (\Protection' {protectionArn} -> protectionArn) (\s@Protection' {} a -> s {protectionArn = a} :: Protection)

-- | The ARN (Amazon Resource Name) of the Amazon Web Services resource that
-- is protected.
protection_resourceArn :: Lens.Lens' Protection (Prelude.Maybe Prelude.Text)
protection_resourceArn = Lens.lens (\Protection' {resourceArn} -> resourceArn) (\s@Protection' {} a -> s {resourceArn = a} :: Protection)

instance Data.FromJSON Protection where
  parseJSON =
    Data.withObject
      "Protection"
      ( \x ->
          Protection'
            Prelude.<$> ( x
                            Data..:? "ApplicationLayerAutomaticResponseConfiguration"
                        )
            Prelude.<*> (x Data..:? "HealthCheckIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ProtectionArn")
            Prelude.<*> (x Data..:? "ResourceArn")
      )

instance Prelude.Hashable Protection where
  hashWithSalt _salt Protection' {..} =
    _salt
      `Prelude.hashWithSalt` applicationLayerAutomaticResponseConfiguration
      `Prelude.hashWithSalt` healthCheckIds
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` protectionArn
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData Protection where
  rnf Protection' {..} =
    Prelude.rnf
      applicationLayerAutomaticResponseConfiguration
      `Prelude.seq` Prelude.rnf healthCheckIds
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf protectionArn
      `Prelude.seq` Prelude.rnf resourceArn
