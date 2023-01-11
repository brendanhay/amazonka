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
-- Module      : Amazonka.GroundStation.Types.AntennaDemodDecodeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.AntennaDemodDecodeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an antenna demod decode @Config@ used in a contact.
--
-- /See:/ 'newAntennaDemodDecodeDetails' smart constructor.
data AntennaDemodDecodeDetails = AntennaDemodDecodeDetails'
  { -- | Name of an antenna demod decode output node used in a contact.
    outputNode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AntennaDemodDecodeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputNode', 'antennaDemodDecodeDetails_outputNode' - Name of an antenna demod decode output node used in a contact.
newAntennaDemodDecodeDetails ::
  AntennaDemodDecodeDetails
newAntennaDemodDecodeDetails =
  AntennaDemodDecodeDetails'
    { outputNode =
        Prelude.Nothing
    }

-- | Name of an antenna demod decode output node used in a contact.
antennaDemodDecodeDetails_outputNode :: Lens.Lens' AntennaDemodDecodeDetails (Prelude.Maybe Prelude.Text)
antennaDemodDecodeDetails_outputNode = Lens.lens (\AntennaDemodDecodeDetails' {outputNode} -> outputNode) (\s@AntennaDemodDecodeDetails' {} a -> s {outputNode = a} :: AntennaDemodDecodeDetails)

instance Data.FromJSON AntennaDemodDecodeDetails where
  parseJSON =
    Data.withObject
      "AntennaDemodDecodeDetails"
      ( \x ->
          AntennaDemodDecodeDetails'
            Prelude.<$> (x Data..:? "outputNode")
      )

instance Prelude.Hashable AntennaDemodDecodeDetails where
  hashWithSalt _salt AntennaDemodDecodeDetails' {..} =
    _salt `Prelude.hashWithSalt` outputNode

instance Prelude.NFData AntennaDemodDecodeDetails where
  rnf AntennaDemodDecodeDetails' {..} =
    Prelude.rnf outputNode
