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
-- Module      : Amazonka.MediaTailor.Types.SlateSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.SlateSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Slate VOD source configuration.
--
-- /See:/ 'newSlateSource' smart constructor.
data SlateSource = SlateSource'
  { -- | The slate VOD source name. The VOD source must already exist in a source
    -- location before it can be used for slate.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the source location where the slate VOD source is stored.
    sourceLocationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlateSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vodSourceName', 'slateSource_vodSourceName' - The slate VOD source name. The VOD source must already exist in a source
-- location before it can be used for slate.
--
-- 'sourceLocationName', 'slateSource_sourceLocationName' - The name of the source location where the slate VOD source is stored.
newSlateSource ::
  SlateSource
newSlateSource =
  SlateSource'
    { vodSourceName = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing
    }

-- | The slate VOD source name. The VOD source must already exist in a source
-- location before it can be used for slate.
slateSource_vodSourceName :: Lens.Lens' SlateSource (Prelude.Maybe Prelude.Text)
slateSource_vodSourceName = Lens.lens (\SlateSource' {vodSourceName} -> vodSourceName) (\s@SlateSource' {} a -> s {vodSourceName = a} :: SlateSource)

-- | The name of the source location where the slate VOD source is stored.
slateSource_sourceLocationName :: Lens.Lens' SlateSource (Prelude.Maybe Prelude.Text)
slateSource_sourceLocationName = Lens.lens (\SlateSource' {sourceLocationName} -> sourceLocationName) (\s@SlateSource' {} a -> s {sourceLocationName = a} :: SlateSource)

instance Core.FromJSON SlateSource where
  parseJSON =
    Core.withObject
      "SlateSource"
      ( \x ->
          SlateSource'
            Prelude.<$> (x Core..:? "VodSourceName")
            Prelude.<*> (x Core..:? "SourceLocationName")
      )

instance Prelude.Hashable SlateSource where
  hashWithSalt _salt SlateSource' {..} =
    _salt `Prelude.hashWithSalt` vodSourceName
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData SlateSource where
  rnf SlateSource' {..} =
    Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf sourceLocationName

instance Core.ToJSON SlateSource where
  toJSON SlateSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VodSourceName" Core..=) Prelude.<$> vodSourceName,
            ("SourceLocationName" Core..=)
              Prelude.<$> sourceLocationName
          ]
      )
