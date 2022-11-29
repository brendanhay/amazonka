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
-- Module      : Amazonka.Route53Resolver.Types.ResolverDnssecConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverDnssecConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.ResolverDNSSECValidationStatus

-- | A complex type that contains information about a configuration for
-- DNSSEC validation.
--
-- /See:/ 'newResolverDnssecConfig' smart constructor.
data ResolverDnssecConfig = ResolverDnssecConfig'
  { -- | The ID of the virtual private cloud (VPC) that you\'re configuring the
    -- DNSSEC validation status for.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The owner account ID of the virtual private cloud (VPC) for a
    -- configuration for DNSSEC validation.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The validation status for a DNSSEC configuration. The status can be one
    -- of the following:
    --
    -- -   __ENABLING:__ DNSSEC validation is being enabled but is not
    --     complete.
    --
    -- -   __ENABLED:__ DNSSEC validation is enabled.
    --
    -- -   __DISABLING:__ DNSSEC validation is being disabled but is not
    --     complete.
    --
    -- -   __DISABLED__ DNSSEC validation is disabled.
    validationStatus :: Prelude.Maybe ResolverDNSSECValidationStatus,
    -- | The ID for a configuration for DNSSEC validation.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolverDnssecConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'resolverDnssecConfig_resourceId' - The ID of the virtual private cloud (VPC) that you\'re configuring the
-- DNSSEC validation status for.
--
-- 'ownerId', 'resolverDnssecConfig_ownerId' - The owner account ID of the virtual private cloud (VPC) for a
-- configuration for DNSSEC validation.
--
-- 'validationStatus', 'resolverDnssecConfig_validationStatus' - The validation status for a DNSSEC configuration. The status can be one
-- of the following:
--
-- -   __ENABLING:__ DNSSEC validation is being enabled but is not
--     complete.
--
-- -   __ENABLED:__ DNSSEC validation is enabled.
--
-- -   __DISABLING:__ DNSSEC validation is being disabled but is not
--     complete.
--
-- -   __DISABLED__ DNSSEC validation is disabled.
--
-- 'id', 'resolverDnssecConfig_id' - The ID for a configuration for DNSSEC validation.
newResolverDnssecConfig ::
  ResolverDnssecConfig
newResolverDnssecConfig =
  ResolverDnssecConfig'
    { resourceId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      validationStatus = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The ID of the virtual private cloud (VPC) that you\'re configuring the
-- DNSSEC validation status for.
resolverDnssecConfig_resourceId :: Lens.Lens' ResolverDnssecConfig (Prelude.Maybe Prelude.Text)
resolverDnssecConfig_resourceId = Lens.lens (\ResolverDnssecConfig' {resourceId} -> resourceId) (\s@ResolverDnssecConfig' {} a -> s {resourceId = a} :: ResolverDnssecConfig)

-- | The owner account ID of the virtual private cloud (VPC) for a
-- configuration for DNSSEC validation.
resolverDnssecConfig_ownerId :: Lens.Lens' ResolverDnssecConfig (Prelude.Maybe Prelude.Text)
resolverDnssecConfig_ownerId = Lens.lens (\ResolverDnssecConfig' {ownerId} -> ownerId) (\s@ResolverDnssecConfig' {} a -> s {ownerId = a} :: ResolverDnssecConfig)

-- | The validation status for a DNSSEC configuration. The status can be one
-- of the following:
--
-- -   __ENABLING:__ DNSSEC validation is being enabled but is not
--     complete.
--
-- -   __ENABLED:__ DNSSEC validation is enabled.
--
-- -   __DISABLING:__ DNSSEC validation is being disabled but is not
--     complete.
--
-- -   __DISABLED__ DNSSEC validation is disabled.
resolverDnssecConfig_validationStatus :: Lens.Lens' ResolverDnssecConfig (Prelude.Maybe ResolverDNSSECValidationStatus)
resolverDnssecConfig_validationStatus = Lens.lens (\ResolverDnssecConfig' {validationStatus} -> validationStatus) (\s@ResolverDnssecConfig' {} a -> s {validationStatus = a} :: ResolverDnssecConfig)

-- | The ID for a configuration for DNSSEC validation.
resolverDnssecConfig_id :: Lens.Lens' ResolverDnssecConfig (Prelude.Maybe Prelude.Text)
resolverDnssecConfig_id = Lens.lens (\ResolverDnssecConfig' {id} -> id) (\s@ResolverDnssecConfig' {} a -> s {id = a} :: ResolverDnssecConfig)

instance Core.FromJSON ResolverDnssecConfig where
  parseJSON =
    Core.withObject
      "ResolverDnssecConfig"
      ( \x ->
          ResolverDnssecConfig'
            Prelude.<$> (x Core..:? "ResourceId")
            Prelude.<*> (x Core..:? "OwnerId")
            Prelude.<*> (x Core..:? "ValidationStatus")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable ResolverDnssecConfig where
  hashWithSalt _salt ResolverDnssecConfig' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` validationStatus
      `Prelude.hashWithSalt` id

instance Prelude.NFData ResolverDnssecConfig where
  rnf ResolverDnssecConfig' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf validationStatus
      `Prelude.seq` Prelude.rnf id
