{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the placement of an instance.
--
-- /See:/ 'newPlacementResponse' smart constructor.
data PlacementResponse = PlacementResponse'
  { -- | The name of the placement group that the instance is in.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  PlacementResponse' {groupName = Prelude.Nothing}

-- | The name of the placement group that the instance is in.
placementResponse_groupName :: Lens.Lens' PlacementResponse (Prelude.Maybe Prelude.Text)
placementResponse_groupName = Lens.lens (\PlacementResponse' {groupName} -> groupName) (\s@PlacementResponse' {} a -> s {groupName = a} :: PlacementResponse)

instance Prelude.FromXML PlacementResponse where
  parseXML x =
    PlacementResponse'
      Prelude.<$> (x Prelude..@? "groupName")

instance Prelude.Hashable PlacementResponse

instance Prelude.NFData PlacementResponse
