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
-- Module      : Network.AWS.EC2.Types.PlacementGroupInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementGroupInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PlacementGroupStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the placement group support of the instance type.
--
-- /See:/ 'newPlacementGroupInfo' smart constructor.
data PlacementGroupInfo = PlacementGroupInfo'
  { -- | The supported placement group types.
    supportedStrategies :: Prelude.Maybe [PlacementGroupStrategy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PlacementGroupInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'supportedStrategies', 'placementGroupInfo_supportedStrategies' - The supported placement group types.
newPlacementGroupInfo ::
  PlacementGroupInfo
newPlacementGroupInfo =
  PlacementGroupInfo'
    { supportedStrategies =
        Prelude.Nothing
    }

-- | The supported placement group types.
placementGroupInfo_supportedStrategies :: Lens.Lens' PlacementGroupInfo (Prelude.Maybe [PlacementGroupStrategy])
placementGroupInfo_supportedStrategies = Lens.lens (\PlacementGroupInfo' {supportedStrategies} -> supportedStrategies) (\s@PlacementGroupInfo' {} a -> s {supportedStrategies = a} :: PlacementGroupInfo) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML PlacementGroupInfo where
  parseXML x =
    PlacementGroupInfo'
      Prelude.<$> ( x Prelude..@? "supportedStrategies"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable PlacementGroupInfo

instance Prelude.NFData PlacementGroupInfo
