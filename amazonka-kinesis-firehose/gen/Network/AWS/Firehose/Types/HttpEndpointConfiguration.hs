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
-- Module      : Network.AWS.Firehose.Types.HttpEndpointConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HttpEndpointConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the configuration of the HTTP endpoint to which Kinesis
-- Firehose delivers data.
--
-- /See:/ 'newHttpEndpointConfiguration' smart constructor.
data HttpEndpointConfiguration = HttpEndpointConfiguration'
  { -- | The access key required for Kinesis Firehose to authenticate with the
    -- HTTP endpoint selected as the destination.
    accessKey :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The name of the HTTP endpoint selected as the destination.
    name :: Core.Maybe Core.Text,
    -- | The URL of the HTTP endpoint selected as the destination.
    url :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'HttpEndpointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKey', 'httpEndpointConfiguration_accessKey' - The access key required for Kinesis Firehose to authenticate with the
-- HTTP endpoint selected as the destination.
--
-- 'name', 'httpEndpointConfiguration_name' - The name of the HTTP endpoint selected as the destination.
--
-- 'url', 'httpEndpointConfiguration_url' - The URL of the HTTP endpoint selected as the destination.
newHttpEndpointConfiguration ::
  -- | 'url'
  Core.Text ->
  HttpEndpointConfiguration
newHttpEndpointConfiguration pUrl_ =
  HttpEndpointConfiguration'
    { accessKey =
        Core.Nothing,
      name = Core.Nothing,
      url = Core._Sensitive Lens.# pUrl_
    }

-- | The access key required for Kinesis Firehose to authenticate with the
-- HTTP endpoint selected as the destination.
httpEndpointConfiguration_accessKey :: Lens.Lens' HttpEndpointConfiguration (Core.Maybe Core.Text)
httpEndpointConfiguration_accessKey = Lens.lens (\HttpEndpointConfiguration' {accessKey} -> accessKey) (\s@HttpEndpointConfiguration' {} a -> s {accessKey = a} :: HttpEndpointConfiguration) Core.. Lens.mapping Core._Sensitive

-- | The name of the HTTP endpoint selected as the destination.
httpEndpointConfiguration_name :: Lens.Lens' HttpEndpointConfiguration (Core.Maybe Core.Text)
httpEndpointConfiguration_name = Lens.lens (\HttpEndpointConfiguration' {name} -> name) (\s@HttpEndpointConfiguration' {} a -> s {name = a} :: HttpEndpointConfiguration)

-- | The URL of the HTTP endpoint selected as the destination.
httpEndpointConfiguration_url :: Lens.Lens' HttpEndpointConfiguration Core.Text
httpEndpointConfiguration_url = Lens.lens (\HttpEndpointConfiguration' {url} -> url) (\s@HttpEndpointConfiguration' {} a -> s {url = a} :: HttpEndpointConfiguration) Core.. Core._Sensitive

instance Core.Hashable HttpEndpointConfiguration

instance Core.NFData HttpEndpointConfiguration

instance Core.ToJSON HttpEndpointConfiguration where
  toJSON HttpEndpointConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessKey" Core..=) Core.<$> accessKey,
            ("Name" Core..=) Core.<$> name,
            Core.Just ("Url" Core..= url)
          ]
      )
