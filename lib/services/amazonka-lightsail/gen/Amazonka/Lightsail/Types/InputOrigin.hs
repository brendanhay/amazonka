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
-- Module      : Amazonka.Lightsail.Types.InputOrigin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InputOrigin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.OriginProtocolPolicyEnum
import Amazonka.Lightsail.Types.RegionName
import qualified Amazonka.Prelude as Prelude

-- | Describes the origin resource of an Amazon Lightsail content delivery
-- network (CDN) distribution.
--
-- An origin can be a Lightsail instance, bucket, or load balancer. A
-- distribution pulls content from an origin, caches it, and serves it to
-- viewers via a worldwide network of edge servers.
--
-- /See:/ 'newInputOrigin' smart constructor.
data InputOrigin = InputOrigin'
  { -- | The name of the origin resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The protocol that your Amazon Lightsail distribution uses when
    -- establishing a connection with your origin to pull content.
    protocolPolicy :: Prelude.Maybe OriginProtocolPolicyEnum,
    -- | The AWS Region name of the origin resource.
    regionName :: Prelude.Maybe RegionName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputOrigin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'inputOrigin_name' - The name of the origin resource.
--
-- 'protocolPolicy', 'inputOrigin_protocolPolicy' - The protocol that your Amazon Lightsail distribution uses when
-- establishing a connection with your origin to pull content.
--
-- 'regionName', 'inputOrigin_regionName' - The AWS Region name of the origin resource.
newInputOrigin ::
  InputOrigin
newInputOrigin =
  InputOrigin'
    { name = Prelude.Nothing,
      protocolPolicy = Prelude.Nothing,
      regionName = Prelude.Nothing
    }

-- | The name of the origin resource.
inputOrigin_name :: Lens.Lens' InputOrigin (Prelude.Maybe Prelude.Text)
inputOrigin_name = Lens.lens (\InputOrigin' {name} -> name) (\s@InputOrigin' {} a -> s {name = a} :: InputOrigin)

-- | The protocol that your Amazon Lightsail distribution uses when
-- establishing a connection with your origin to pull content.
inputOrigin_protocolPolicy :: Lens.Lens' InputOrigin (Prelude.Maybe OriginProtocolPolicyEnum)
inputOrigin_protocolPolicy = Lens.lens (\InputOrigin' {protocolPolicy} -> protocolPolicy) (\s@InputOrigin' {} a -> s {protocolPolicy = a} :: InputOrigin)

-- | The AWS Region name of the origin resource.
inputOrigin_regionName :: Lens.Lens' InputOrigin (Prelude.Maybe RegionName)
inputOrigin_regionName = Lens.lens (\InputOrigin' {regionName} -> regionName) (\s@InputOrigin' {} a -> s {regionName = a} :: InputOrigin)

instance Prelude.Hashable InputOrigin where
  hashWithSalt _salt InputOrigin' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` protocolPolicy
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData InputOrigin where
  rnf InputOrigin' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf protocolPolicy `Prelude.seq`
        Prelude.rnf regionName

instance Data.ToJSON InputOrigin where
  toJSON InputOrigin' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("protocolPolicy" Data..=)
              Prelude.<$> protocolPolicy,
            ("regionName" Data..=) Prelude.<$> regionName
          ]
      )
