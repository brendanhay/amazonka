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
-- Module      : Network.AWS.Firehose.Types.HttpEndpointDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HttpEndpointDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the HTTP endpoint selected as the destination.
--
-- /See:/ 'newHttpEndpointDescription' smart constructor.
data HttpEndpointDescription = HttpEndpointDescription'
  { -- | The name of the HTTP endpoint selected as the destination.
    name :: Core.Maybe Core.Text,
    -- | The URL of the HTTP endpoint selected as the destination.
    url :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'HttpEndpointDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'httpEndpointDescription_name' - The name of the HTTP endpoint selected as the destination.
--
-- 'url', 'httpEndpointDescription_url' - The URL of the HTTP endpoint selected as the destination.
newHttpEndpointDescription ::
  HttpEndpointDescription
newHttpEndpointDescription =
  HttpEndpointDescription'
    { name = Core.Nothing,
      url = Core.Nothing
    }

-- | The name of the HTTP endpoint selected as the destination.
httpEndpointDescription_name :: Lens.Lens' HttpEndpointDescription (Core.Maybe Core.Text)
httpEndpointDescription_name = Lens.lens (\HttpEndpointDescription' {name} -> name) (\s@HttpEndpointDescription' {} a -> s {name = a} :: HttpEndpointDescription)

-- | The URL of the HTTP endpoint selected as the destination.
httpEndpointDescription_url :: Lens.Lens' HttpEndpointDescription (Core.Maybe Core.Text)
httpEndpointDescription_url = Lens.lens (\HttpEndpointDescription' {url} -> url) (\s@HttpEndpointDescription' {} a -> s {url = a} :: HttpEndpointDescription) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON HttpEndpointDescription where
  parseJSON =
    Core.withObject
      "HttpEndpointDescription"
      ( \x ->
          HttpEndpointDescription'
            Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "Url")
      )

instance Core.Hashable HttpEndpointDescription

instance Core.NFData HttpEndpointDescription
