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
-- Module      : Network.AWS.Lightsail.Types.DestinationInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DestinationInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the destination of a record.
--
-- /See:/ 'newDestinationInfo' smart constructor.
data DestinationInfo = DestinationInfo'
  { -- | The destination service of the record.
    service :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource created at the destination.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'destinationInfo_service' - The destination service of the record.
--
-- 'id', 'destinationInfo_id' - The ID of the resource created at the destination.
newDestinationInfo ::
  DestinationInfo
newDestinationInfo =
  DestinationInfo'
    { service = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The destination service of the record.
destinationInfo_service :: Lens.Lens' DestinationInfo (Prelude.Maybe Prelude.Text)
destinationInfo_service = Lens.lens (\DestinationInfo' {service} -> service) (\s@DestinationInfo' {} a -> s {service = a} :: DestinationInfo)

-- | The ID of the resource created at the destination.
destinationInfo_id :: Lens.Lens' DestinationInfo (Prelude.Maybe Prelude.Text)
destinationInfo_id = Lens.lens (\DestinationInfo' {id} -> id) (\s@DestinationInfo' {} a -> s {id = a} :: DestinationInfo)

instance Core.FromJSON DestinationInfo where
  parseJSON =
    Core.withObject
      "DestinationInfo"
      ( \x ->
          DestinationInfo'
            Prelude.<$> (x Core..:? "service") Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable DestinationInfo

instance Prelude.NFData DestinationInfo
