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
-- Module      : Network.AWS.EC2.Types.PlacementResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the placement of an instance.
--
-- /See:/ 'newPlacementResponse' smart constructor.
data PlacementResponse = PlacementResponse'
  { -- | The name of the placement group that the instance is in.
    groupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PlacementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'placementResponse_groupName' - The name of the placement group that the instance is in.
newPlacementResponse ::
  PlacementResponse
newPlacementResponse =
  PlacementResponse' {groupName = Core.Nothing}

-- | The name of the placement group that the instance is in.
placementResponse_groupName :: Lens.Lens' PlacementResponse (Core.Maybe Core.Text)
placementResponse_groupName = Lens.lens (\PlacementResponse' {groupName} -> groupName) (\s@PlacementResponse' {} a -> s {groupName = a} :: PlacementResponse)

instance Core.FromXML PlacementResponse where
  parseXML x =
    PlacementResponse'
      Core.<$> (x Core..@? "groupName")

instance Core.Hashable PlacementResponse

instance Core.NFData PlacementResponse
