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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.SlateSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Slate VOD source configuration.
--
-- /See:/ 'newSlateSource' smart constructor.
data SlateSource = SlateSource'
  { -- | The name of the source location where the slate VOD source is stored.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The slate VOD source name. The VOD source must already exist in a source
    -- location before it can be used for slate.
    vodSourceName :: Prelude.Maybe Prelude.Text
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
-- 'sourceLocationName', 'slateSource_sourceLocationName' - The name of the source location where the slate VOD source is stored.
--
-- 'vodSourceName', 'slateSource_vodSourceName' - The slate VOD source name. The VOD source must already exist in a source
-- location before it can be used for slate.
newSlateSource ::
  SlateSource
newSlateSource =
  SlateSource'
    { sourceLocationName = Prelude.Nothing,
      vodSourceName = Prelude.Nothing
    }

-- | The name of the source location where the slate VOD source is stored.
slateSource_sourceLocationName :: Lens.Lens' SlateSource (Prelude.Maybe Prelude.Text)
slateSource_sourceLocationName = Lens.lens (\SlateSource' {sourceLocationName} -> sourceLocationName) (\s@SlateSource' {} a -> s {sourceLocationName = a} :: SlateSource)

-- | The slate VOD source name. The VOD source must already exist in a source
-- location before it can be used for slate.
slateSource_vodSourceName :: Lens.Lens' SlateSource (Prelude.Maybe Prelude.Text)
slateSource_vodSourceName = Lens.lens (\SlateSource' {vodSourceName} -> vodSourceName) (\s@SlateSource' {} a -> s {vodSourceName = a} :: SlateSource)

instance Data.FromJSON SlateSource where
  parseJSON =
    Data.withObject
      "SlateSource"
      ( \x ->
          SlateSource'
            Prelude.<$> (x Data..:? "SourceLocationName")
            Prelude.<*> (x Data..:? "VodSourceName")
      )

instance Prelude.Hashable SlateSource where
  hashWithSalt _salt SlateSource' {..} =
    _salt
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` vodSourceName

instance Prelude.NFData SlateSource where
  rnf SlateSource' {..} =
    Prelude.rnf sourceLocationName `Prelude.seq`
      Prelude.rnf vodSourceName

instance Data.ToJSON SlateSource where
  toJSON SlateSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourceLocationName" Data..=)
              Prelude.<$> sourceLocationName,
            ("VodSourceName" Data..=) Prelude.<$> vodSourceName
          ]
      )
