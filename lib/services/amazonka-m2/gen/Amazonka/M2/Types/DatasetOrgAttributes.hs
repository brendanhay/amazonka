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
-- Module      : Amazonka.M2.Types.DatasetOrgAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DatasetOrgAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.GdgAttributes
import Amazonka.M2.Types.VsamAttributes
import qualified Amazonka.Prelude as Prelude

-- | Additional details about the data set. Different attributes correspond
-- to different data set organizations. The values are populated based on
-- datasetOrg, storageType and backend (Blu Age or Micro Focus).
--
-- /See:/ 'newDatasetOrgAttributes' smart constructor.
data DatasetOrgAttributes = DatasetOrgAttributes'
  { -- | The generation data group of the data set.
    gdg :: Prelude.Maybe GdgAttributes,
    -- | The details of a VSAM data set.
    vsam :: Prelude.Maybe VsamAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetOrgAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gdg', 'datasetOrgAttributes_gdg' - The generation data group of the data set.
--
-- 'vsam', 'datasetOrgAttributes_vsam' - The details of a VSAM data set.
newDatasetOrgAttributes ::
  DatasetOrgAttributes
newDatasetOrgAttributes =
  DatasetOrgAttributes'
    { gdg = Prelude.Nothing,
      vsam = Prelude.Nothing
    }

-- | The generation data group of the data set.
datasetOrgAttributes_gdg :: Lens.Lens' DatasetOrgAttributes (Prelude.Maybe GdgAttributes)
datasetOrgAttributes_gdg = Lens.lens (\DatasetOrgAttributes' {gdg} -> gdg) (\s@DatasetOrgAttributes' {} a -> s {gdg = a} :: DatasetOrgAttributes)

-- | The details of a VSAM data set.
datasetOrgAttributes_vsam :: Lens.Lens' DatasetOrgAttributes (Prelude.Maybe VsamAttributes)
datasetOrgAttributes_vsam = Lens.lens (\DatasetOrgAttributes' {vsam} -> vsam) (\s@DatasetOrgAttributes' {} a -> s {vsam = a} :: DatasetOrgAttributes)

instance Prelude.Hashable DatasetOrgAttributes where
  hashWithSalt _salt DatasetOrgAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` gdg
      `Prelude.hashWithSalt` vsam

instance Prelude.NFData DatasetOrgAttributes where
  rnf DatasetOrgAttributes' {..} =
    Prelude.rnf gdg `Prelude.seq` Prelude.rnf vsam

instance Data.ToJSON DatasetOrgAttributes where
  toJSON DatasetOrgAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("gdg" Data..=) Prelude.<$> gdg,
            ("vsam" Data..=) Prelude.<$> vsam
          ]
      )
