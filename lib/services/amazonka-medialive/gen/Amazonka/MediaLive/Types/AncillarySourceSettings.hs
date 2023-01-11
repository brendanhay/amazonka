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
-- Module      : Amazonka.MediaLive.Types.AncillarySourceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AncillarySourceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Ancillary Source Settings
--
-- /See:/ 'newAncillarySourceSettings' smart constructor.
data AncillarySourceSettings = AncillarySourceSettings'
  { -- | Specifies the number (1 to 4) of the captions channel you want to
    -- extract from the ancillary captions. If you plan to convert the
    -- ancillary captions to another format, complete this field. If you plan
    -- to choose Embedded as the captions destination in the output (to pass
    -- through all the channels in the ancillary captions), leave this field
    -- blank because MediaLive ignores the field.
    sourceAncillaryChannelNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AncillarySourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceAncillaryChannelNumber', 'ancillarySourceSettings_sourceAncillaryChannelNumber' - Specifies the number (1 to 4) of the captions channel you want to
-- extract from the ancillary captions. If you plan to convert the
-- ancillary captions to another format, complete this field. If you plan
-- to choose Embedded as the captions destination in the output (to pass
-- through all the channels in the ancillary captions), leave this field
-- blank because MediaLive ignores the field.
newAncillarySourceSettings ::
  AncillarySourceSettings
newAncillarySourceSettings =
  AncillarySourceSettings'
    { sourceAncillaryChannelNumber =
        Prelude.Nothing
    }

-- | Specifies the number (1 to 4) of the captions channel you want to
-- extract from the ancillary captions. If you plan to convert the
-- ancillary captions to another format, complete this field. If you plan
-- to choose Embedded as the captions destination in the output (to pass
-- through all the channels in the ancillary captions), leave this field
-- blank because MediaLive ignores the field.
ancillarySourceSettings_sourceAncillaryChannelNumber :: Lens.Lens' AncillarySourceSettings (Prelude.Maybe Prelude.Natural)
ancillarySourceSettings_sourceAncillaryChannelNumber = Lens.lens (\AncillarySourceSettings' {sourceAncillaryChannelNumber} -> sourceAncillaryChannelNumber) (\s@AncillarySourceSettings' {} a -> s {sourceAncillaryChannelNumber = a} :: AncillarySourceSettings)

instance Data.FromJSON AncillarySourceSettings where
  parseJSON =
    Data.withObject
      "AncillarySourceSettings"
      ( \x ->
          AncillarySourceSettings'
            Prelude.<$> (x Data..:? "sourceAncillaryChannelNumber")
      )

instance Prelude.Hashable AncillarySourceSettings where
  hashWithSalt _salt AncillarySourceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` sourceAncillaryChannelNumber

instance Prelude.NFData AncillarySourceSettings where
  rnf AncillarySourceSettings' {..} =
    Prelude.rnf sourceAncillaryChannelNumber

instance Data.ToJSON AncillarySourceSettings where
  toJSON AncillarySourceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sourceAncillaryChannelNumber" Data..=)
              Prelude.<$> sourceAncillaryChannelNumber
          ]
      )
