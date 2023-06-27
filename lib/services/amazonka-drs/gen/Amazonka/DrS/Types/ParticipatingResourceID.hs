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
-- Module      : Amazonka.DrS.Types.ParticipatingResourceID
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ParticipatingResourceID where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | ID of a resource participating in an asynchronous Job.
--
-- /See:/ 'newParticipatingResourceID' smart constructor.
data ParticipatingResourceID = ParticipatingResourceID'
  { -- | Source Network ID.
    sourceNetworkID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipatingResourceID' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetworkID', 'participatingResourceID_sourceNetworkID' - Source Network ID.
newParticipatingResourceID ::
  ParticipatingResourceID
newParticipatingResourceID =
  ParticipatingResourceID'
    { sourceNetworkID =
        Prelude.Nothing
    }

-- | Source Network ID.
participatingResourceID_sourceNetworkID :: Lens.Lens' ParticipatingResourceID (Prelude.Maybe Prelude.Text)
participatingResourceID_sourceNetworkID = Lens.lens (\ParticipatingResourceID' {sourceNetworkID} -> sourceNetworkID) (\s@ParticipatingResourceID' {} a -> s {sourceNetworkID = a} :: ParticipatingResourceID)

instance Data.FromJSON ParticipatingResourceID where
  parseJSON =
    Data.withObject
      "ParticipatingResourceID"
      ( \x ->
          ParticipatingResourceID'
            Prelude.<$> (x Data..:? "sourceNetworkID")
      )

instance Prelude.Hashable ParticipatingResourceID where
  hashWithSalt _salt ParticipatingResourceID' {..} =
    _salt `Prelude.hashWithSalt` sourceNetworkID

instance Prelude.NFData ParticipatingResourceID where
  rnf ParticipatingResourceID' {..} =
    Prelude.rnf sourceNetworkID
