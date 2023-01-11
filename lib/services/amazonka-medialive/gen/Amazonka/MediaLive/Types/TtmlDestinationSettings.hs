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
-- Module      : Amazonka.MediaLive.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TtmlDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.TtmlDestinationStyleControl
import qualified Amazonka.Prelude as Prelude

-- | Ttml Destination Settings
--
-- /See:/ 'newTtmlDestinationSettings' smart constructor.
data TtmlDestinationSettings = TtmlDestinationSettings'
  { -- | This field is not currently supported and will not affect the output
    -- styling. Leave the default value.
    styleControl :: Prelude.Maybe TtmlDestinationStyleControl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TtmlDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'styleControl', 'ttmlDestinationSettings_styleControl' - This field is not currently supported and will not affect the output
-- styling. Leave the default value.
newTtmlDestinationSettings ::
  TtmlDestinationSettings
newTtmlDestinationSettings =
  TtmlDestinationSettings'
    { styleControl =
        Prelude.Nothing
    }

-- | This field is not currently supported and will not affect the output
-- styling. Leave the default value.
ttmlDestinationSettings_styleControl :: Lens.Lens' TtmlDestinationSettings (Prelude.Maybe TtmlDestinationStyleControl)
ttmlDestinationSettings_styleControl = Lens.lens (\TtmlDestinationSettings' {styleControl} -> styleControl) (\s@TtmlDestinationSettings' {} a -> s {styleControl = a} :: TtmlDestinationSettings)

instance Data.FromJSON TtmlDestinationSettings where
  parseJSON =
    Data.withObject
      "TtmlDestinationSettings"
      ( \x ->
          TtmlDestinationSettings'
            Prelude.<$> (x Data..:? "styleControl")
      )

instance Prelude.Hashable TtmlDestinationSettings where
  hashWithSalt _salt TtmlDestinationSettings' {..} =
    _salt `Prelude.hashWithSalt` styleControl

instance Prelude.NFData TtmlDestinationSettings where
  rnf TtmlDestinationSettings' {..} =
    Prelude.rnf styleControl

instance Data.ToJSON TtmlDestinationSettings where
  toJSON TtmlDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("styleControl" Data..=) Prelude.<$> styleControl]
      )
