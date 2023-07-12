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
-- Module      : Amazonka.Lightsail.Types.DestinationInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DestinationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination of a record.
--
-- /See:/ 'newDestinationInfo' smart constructor.
data DestinationInfo = DestinationInfo'
  { -- | The ID of the resource created at the destination.
    id :: Prelude.Maybe Prelude.Text,
    -- | The destination service of the record.
    service :: Prelude.Maybe Prelude.Text
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
-- 'id', 'destinationInfo_id' - The ID of the resource created at the destination.
--
-- 'service', 'destinationInfo_service' - The destination service of the record.
newDestinationInfo ::
  DestinationInfo
newDestinationInfo =
  DestinationInfo'
    { id = Prelude.Nothing,
      service = Prelude.Nothing
    }

-- | The ID of the resource created at the destination.
destinationInfo_id :: Lens.Lens' DestinationInfo (Prelude.Maybe Prelude.Text)
destinationInfo_id = Lens.lens (\DestinationInfo' {id} -> id) (\s@DestinationInfo' {} a -> s {id = a} :: DestinationInfo)

-- | The destination service of the record.
destinationInfo_service :: Lens.Lens' DestinationInfo (Prelude.Maybe Prelude.Text)
destinationInfo_service = Lens.lens (\DestinationInfo' {service} -> service) (\s@DestinationInfo' {} a -> s {service = a} :: DestinationInfo)

instance Data.FromJSON DestinationInfo where
  parseJSON =
    Data.withObject
      "DestinationInfo"
      ( \x ->
          DestinationInfo'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "service")
      )

instance Prelude.Hashable DestinationInfo where
  hashWithSalt _salt DestinationInfo' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` service

instance Prelude.NFData DestinationInfo where
  rnf DestinationInfo' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf service
