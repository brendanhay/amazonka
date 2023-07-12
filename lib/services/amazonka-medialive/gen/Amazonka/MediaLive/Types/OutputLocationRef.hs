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
-- Module      : Amazonka.MediaLive.Types.OutputLocationRef
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.OutputLocationRef where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Reference to an OutputDestination ID defined in the channel
--
-- /See:/ 'newOutputLocationRef' smart constructor.
data OutputLocationRef = OutputLocationRef'
  { destinationRefId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputLocationRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationRefId', 'outputLocationRef_destinationRefId' - Undocumented member.
newOutputLocationRef ::
  OutputLocationRef
newOutputLocationRef =
  OutputLocationRef'
    { destinationRefId =
        Prelude.Nothing
    }

-- | Undocumented member.
outputLocationRef_destinationRefId :: Lens.Lens' OutputLocationRef (Prelude.Maybe Prelude.Text)
outputLocationRef_destinationRefId = Lens.lens (\OutputLocationRef' {destinationRefId} -> destinationRefId) (\s@OutputLocationRef' {} a -> s {destinationRefId = a} :: OutputLocationRef)

instance Data.FromJSON OutputLocationRef where
  parseJSON =
    Data.withObject
      "OutputLocationRef"
      ( \x ->
          OutputLocationRef'
            Prelude.<$> (x Data..:? "destinationRefId")
      )

instance Prelude.Hashable OutputLocationRef where
  hashWithSalt _salt OutputLocationRef' {..} =
    _salt `Prelude.hashWithSalt` destinationRefId

instance Prelude.NFData OutputLocationRef where
  rnf OutputLocationRef' {..} =
    Prelude.rnf destinationRefId

instance Data.ToJSON OutputLocationRef where
  toJSON OutputLocationRef' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destinationRefId" Data..=)
              Prelude.<$> destinationRefId
          ]
      )
