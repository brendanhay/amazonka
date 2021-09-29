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
-- Module      : Network.AWS.Shield.Types.Protection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Protection where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents a resource that is under DDoS protection.
--
-- /See:/ 'newProtection' smart constructor.
data Protection = Protection'
  { -- | The ARN (Amazon Resource Name) of the Amazon Web Services resource that
    -- is protected.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the protection.
    protectionArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) of the protection.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the protection. For example, @My CloudFront distributions@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) for the Route 53 health check that\'s
    -- associated with the protection.
    healthCheckIds :: Prelude.Maybe [Prelude.Text]
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
-- 'resourceArn', 'protection_resourceArn' - The ARN (Amazon Resource Name) of the Amazon Web Services resource that
-- is protected.
--
-- 'protectionArn', 'protection_protectionArn' - The ARN (Amazon Resource Name) of the protection.
--
-- 'id', 'protection_id' - The unique identifier (ID) of the protection.
--
-- 'name', 'protection_name' - The name of the protection. For example, @My CloudFront distributions@.
--
-- 'healthCheckIds', 'protection_healthCheckIds' - The unique identifier (ID) for the Route 53 health check that\'s
-- associated with the protection.
newProtection ::
  Protection
newProtection =
  Protection'
    { resourceArn = Prelude.Nothing,
      protectionArn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      healthCheckIds = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the Amazon Web Services resource that
-- is protected.
protection_resourceArn :: Lens.Lens' Protection (Prelude.Maybe Prelude.Text)
protection_resourceArn = Lens.lens (\Protection' {resourceArn} -> resourceArn) (\s@Protection' {} a -> s {resourceArn = a} :: Protection)

-- | The ARN (Amazon Resource Name) of the protection.
protection_protectionArn :: Lens.Lens' Protection (Prelude.Maybe Prelude.Text)
protection_protectionArn = Lens.lens (\Protection' {protectionArn} -> protectionArn) (\s@Protection' {} a -> s {protectionArn = a} :: Protection)

-- | The unique identifier (ID) of the protection.
protection_id :: Lens.Lens' Protection (Prelude.Maybe Prelude.Text)
protection_id = Lens.lens (\Protection' {id} -> id) (\s@Protection' {} a -> s {id = a} :: Protection)

-- | The name of the protection. For example, @My CloudFront distributions@.
protection_name :: Lens.Lens' Protection (Prelude.Maybe Prelude.Text)
protection_name = Lens.lens (\Protection' {name} -> name) (\s@Protection' {} a -> s {name = a} :: Protection)

-- | The unique identifier (ID) for the Route 53 health check that\'s
-- associated with the protection.
protection_healthCheckIds :: Lens.Lens' Protection (Prelude.Maybe [Prelude.Text])
protection_healthCheckIds = Lens.lens (\Protection' {healthCheckIds} -> healthCheckIds) (\s@Protection' {} a -> s {healthCheckIds = a} :: Protection) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON Protection where
  parseJSON =
    Core.withObject
      "Protection"
      ( \x ->
          Protection'
            Prelude.<$> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "ProtectionArn")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> ( x Core..:? "HealthCheckIds"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Protection

instance Prelude.NFData Protection
