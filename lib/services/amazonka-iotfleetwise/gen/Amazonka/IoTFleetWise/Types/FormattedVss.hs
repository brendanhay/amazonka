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
-- Module      : Amazonka.IoTFleetWise.Types.FormattedVss
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.FormattedVss where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Vehicle Signal Specification (VSS) is a precise language used to
-- describe and model signals in vehicle networks. The JSON file collects
-- signal specificiations in a VSS format.
--
-- /See:/ 'newFormattedVss' smart constructor.
data FormattedVss = FormattedVss'
  { -- | Provides the VSS in JSON format.
    vssJson :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormattedVss' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vssJson', 'formattedVss_vssJson' - Provides the VSS in JSON format.
newFormattedVss ::
  FormattedVss
newFormattedVss =
  FormattedVss' {vssJson = Prelude.Nothing}

-- | Provides the VSS in JSON format.
formattedVss_vssJson :: Lens.Lens' FormattedVss (Prelude.Maybe Prelude.Text)
formattedVss_vssJson = Lens.lens (\FormattedVss' {vssJson} -> vssJson) (\s@FormattedVss' {} a -> s {vssJson = a} :: FormattedVss)

instance Prelude.Hashable FormattedVss where
  hashWithSalt _salt FormattedVss' {..} =
    _salt `Prelude.hashWithSalt` vssJson

instance Prelude.NFData FormattedVss where
  rnf FormattedVss' {..} = Prelude.rnf vssJson

instance Data.ToJSON FormattedVss where
  toJSON FormattedVss' {..} =
    Data.object
      ( Prelude.catMaybes
          [("vssJson" Data..=) Prelude.<$> vssJson]
      )
