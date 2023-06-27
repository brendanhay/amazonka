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
-- Module      : Amazonka.Route53Resolver.Types.ResolverConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.ResolverAutodefinedReverseStatus

-- | A complex type that contains information about a Resolver configuration
-- for a VPC.
--
-- /See:/ 'newResolverConfig' smart constructor.
data ResolverConfig = ResolverConfig'
  { -- | The status of whether or not the Resolver will create autodefined rules
    -- for reverse DNS lookups. This is enabled by default. The status can be
    -- one of following:
    --
    -- -   __ENABLING:__ Autodefined rules for reverse DNS lookups are being
    --     enabled but are not complete.
    --
    -- -   __ENABLED:__ Autodefined rules for reverse DNS lookups are enabled.
    --
    -- -   __DISABLING:__ Autodefined rules for reverse DNS lookups are being
    --     disabled but are not complete.
    --
    -- -   __DISABLED:__ Autodefined rules for reverse DNS lookups are
    --     disabled.
    autodefinedReverse :: Prelude.Maybe ResolverAutodefinedReverseStatus,
    -- | ID for the Resolver configuration.
    id :: Prelude.Maybe Prelude.Text,
    -- | The owner account ID of the Amazon Virtual Private Cloud VPC.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Virtual Private Cloud VPC that you\'re configuring
    -- Resolver for.
    resourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolverConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autodefinedReverse', 'resolverConfig_autodefinedReverse' - The status of whether or not the Resolver will create autodefined rules
-- for reverse DNS lookups. This is enabled by default. The status can be
-- one of following:
--
-- -   __ENABLING:__ Autodefined rules for reverse DNS lookups are being
--     enabled but are not complete.
--
-- -   __ENABLED:__ Autodefined rules for reverse DNS lookups are enabled.
--
-- -   __DISABLING:__ Autodefined rules for reverse DNS lookups are being
--     disabled but are not complete.
--
-- -   __DISABLED:__ Autodefined rules for reverse DNS lookups are
--     disabled.
--
-- 'id', 'resolverConfig_id' - ID for the Resolver configuration.
--
-- 'ownerId', 'resolverConfig_ownerId' - The owner account ID of the Amazon Virtual Private Cloud VPC.
--
-- 'resourceId', 'resolverConfig_resourceId' - The ID of the Amazon Virtual Private Cloud VPC that you\'re configuring
-- Resolver for.
newResolverConfig ::
  ResolverConfig
newResolverConfig =
  ResolverConfig'
    { autodefinedReverse =
        Prelude.Nothing,
      id = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      resourceId = Prelude.Nothing
    }

-- | The status of whether or not the Resolver will create autodefined rules
-- for reverse DNS lookups. This is enabled by default. The status can be
-- one of following:
--
-- -   __ENABLING:__ Autodefined rules for reverse DNS lookups are being
--     enabled but are not complete.
--
-- -   __ENABLED:__ Autodefined rules for reverse DNS lookups are enabled.
--
-- -   __DISABLING:__ Autodefined rules for reverse DNS lookups are being
--     disabled but are not complete.
--
-- -   __DISABLED:__ Autodefined rules for reverse DNS lookups are
--     disabled.
resolverConfig_autodefinedReverse :: Lens.Lens' ResolverConfig (Prelude.Maybe ResolverAutodefinedReverseStatus)
resolverConfig_autodefinedReverse = Lens.lens (\ResolverConfig' {autodefinedReverse} -> autodefinedReverse) (\s@ResolverConfig' {} a -> s {autodefinedReverse = a} :: ResolverConfig)

-- | ID for the Resolver configuration.
resolverConfig_id :: Lens.Lens' ResolverConfig (Prelude.Maybe Prelude.Text)
resolverConfig_id = Lens.lens (\ResolverConfig' {id} -> id) (\s@ResolverConfig' {} a -> s {id = a} :: ResolverConfig)

-- | The owner account ID of the Amazon Virtual Private Cloud VPC.
resolverConfig_ownerId :: Lens.Lens' ResolverConfig (Prelude.Maybe Prelude.Text)
resolverConfig_ownerId = Lens.lens (\ResolverConfig' {ownerId} -> ownerId) (\s@ResolverConfig' {} a -> s {ownerId = a} :: ResolverConfig)

-- | The ID of the Amazon Virtual Private Cloud VPC that you\'re configuring
-- Resolver for.
resolverConfig_resourceId :: Lens.Lens' ResolverConfig (Prelude.Maybe Prelude.Text)
resolverConfig_resourceId = Lens.lens (\ResolverConfig' {resourceId} -> resourceId) (\s@ResolverConfig' {} a -> s {resourceId = a} :: ResolverConfig)

instance Data.FromJSON ResolverConfig where
  parseJSON =
    Data.withObject
      "ResolverConfig"
      ( \x ->
          ResolverConfig'
            Prelude.<$> (x Data..:? "AutodefinedReverse")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "ResourceId")
      )

instance Prelude.Hashable ResolverConfig where
  hashWithSalt _salt ResolverConfig' {..} =
    _salt
      `Prelude.hashWithSalt` autodefinedReverse
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData ResolverConfig where
  rnf ResolverConfig' {..} =
    Prelude.rnf autodefinedReverse
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf resourceId
