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
-- Module      : Network.AWS.Connect.Types.RoutingProfileSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains summary information about a routing profile.
--
-- /See:/ 'newRoutingProfileSummary' smart constructor.
data RoutingProfileSummary = RoutingProfileSummary'
  { -- | The Amazon Resource Name (ARN) of the routing profile.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the routing profile.
    id :: Core.Maybe Core.Text,
    -- | The name of the routing profile.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RoutingProfileSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'routingProfileSummary_arn' - The Amazon Resource Name (ARN) of the routing profile.
--
-- 'id', 'routingProfileSummary_id' - The identifier of the routing profile.
--
-- 'name', 'routingProfileSummary_name' - The name of the routing profile.
newRoutingProfileSummary ::
  RoutingProfileSummary
newRoutingProfileSummary =
  RoutingProfileSummary'
    { arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the routing profile.
routingProfileSummary_arn :: Lens.Lens' RoutingProfileSummary (Core.Maybe Core.Text)
routingProfileSummary_arn = Lens.lens (\RoutingProfileSummary' {arn} -> arn) (\s@RoutingProfileSummary' {} a -> s {arn = a} :: RoutingProfileSummary)

-- | The identifier of the routing profile.
routingProfileSummary_id :: Lens.Lens' RoutingProfileSummary (Core.Maybe Core.Text)
routingProfileSummary_id = Lens.lens (\RoutingProfileSummary' {id} -> id) (\s@RoutingProfileSummary' {} a -> s {id = a} :: RoutingProfileSummary)

-- | The name of the routing profile.
routingProfileSummary_name :: Lens.Lens' RoutingProfileSummary (Core.Maybe Core.Text)
routingProfileSummary_name = Lens.lens (\RoutingProfileSummary' {name} -> name) (\s@RoutingProfileSummary' {} a -> s {name = a} :: RoutingProfileSummary)

instance Core.FromJSON RoutingProfileSummary where
  parseJSON =
    Core.withObject
      "RoutingProfileSummary"
      ( \x ->
          RoutingProfileSummary'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable RoutingProfileSummary

instance Core.NFData RoutingProfileSummary
