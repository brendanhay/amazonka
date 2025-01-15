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
-- Module      : Amazonka.QuickSight.Types.AnchorDateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnchorDateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnchorOption

-- | The date configuration of the filter.
--
-- /See:/ 'newAnchorDateConfiguration' smart constructor.
data AnchorDateConfiguration = AnchorDateConfiguration'
  { -- | The options for the date configuration. Choose one of the options below:
    --
    -- -   @NOW@
    anchorOption :: Prelude.Maybe AnchorOption,
    -- | The name of the parameter that is used for the anchor date
    -- configuration.
    parameterName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnchorDateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anchorOption', 'anchorDateConfiguration_anchorOption' - The options for the date configuration. Choose one of the options below:
--
-- -   @NOW@
--
-- 'parameterName', 'anchorDateConfiguration_parameterName' - The name of the parameter that is used for the anchor date
-- configuration.
newAnchorDateConfiguration ::
  AnchorDateConfiguration
newAnchorDateConfiguration =
  AnchorDateConfiguration'
    { anchorOption =
        Prelude.Nothing,
      parameterName = Prelude.Nothing
    }

-- | The options for the date configuration. Choose one of the options below:
--
-- -   @NOW@
anchorDateConfiguration_anchorOption :: Lens.Lens' AnchorDateConfiguration (Prelude.Maybe AnchorOption)
anchorDateConfiguration_anchorOption = Lens.lens (\AnchorDateConfiguration' {anchorOption} -> anchorOption) (\s@AnchorDateConfiguration' {} a -> s {anchorOption = a} :: AnchorDateConfiguration)

-- | The name of the parameter that is used for the anchor date
-- configuration.
anchorDateConfiguration_parameterName :: Lens.Lens' AnchorDateConfiguration (Prelude.Maybe Prelude.Text)
anchorDateConfiguration_parameterName = Lens.lens (\AnchorDateConfiguration' {parameterName} -> parameterName) (\s@AnchorDateConfiguration' {} a -> s {parameterName = a} :: AnchorDateConfiguration)

instance Data.FromJSON AnchorDateConfiguration where
  parseJSON =
    Data.withObject
      "AnchorDateConfiguration"
      ( \x ->
          AnchorDateConfiguration'
            Prelude.<$> (x Data..:? "AnchorOption")
            Prelude.<*> (x Data..:? "ParameterName")
      )

instance Prelude.Hashable AnchorDateConfiguration where
  hashWithSalt _salt AnchorDateConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` anchorOption
      `Prelude.hashWithSalt` parameterName

instance Prelude.NFData AnchorDateConfiguration where
  rnf AnchorDateConfiguration' {..} =
    Prelude.rnf anchorOption `Prelude.seq`
      Prelude.rnf parameterName

instance Data.ToJSON AnchorDateConfiguration where
  toJSON AnchorDateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnchorOption" Data..=) Prelude.<$> anchorOption,
            ("ParameterName" Data..=) Prelude.<$> parameterName
          ]
      )
