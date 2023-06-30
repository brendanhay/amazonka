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
-- Module      : Amazonka.M2.Types.DatasetDetailOrgAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DatasetDetailOrgAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.GdgDetailAttributes
import Amazonka.M2.Types.VsamDetailAttributes
import qualified Amazonka.Prelude as Prelude

-- | Additional details about the data set. Different attributes correspond
-- to different data set organizations. The values are populated based on
-- datasetOrg, storageType and backend (Blu Age or Micro Focus).
--
-- /See:/ 'newDatasetDetailOrgAttributes' smart constructor.
data DatasetDetailOrgAttributes = DatasetDetailOrgAttributes'
  { -- | The generation data group of the data set.
    gdg :: Prelude.Maybe GdgDetailAttributes,
    -- | The details of a VSAM data set.
    vsam :: Prelude.Maybe VsamDetailAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetDetailOrgAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gdg', 'datasetDetailOrgAttributes_gdg' - The generation data group of the data set.
--
-- 'vsam', 'datasetDetailOrgAttributes_vsam' - The details of a VSAM data set.
newDatasetDetailOrgAttributes ::
  DatasetDetailOrgAttributes
newDatasetDetailOrgAttributes =
  DatasetDetailOrgAttributes'
    { gdg = Prelude.Nothing,
      vsam = Prelude.Nothing
    }

-- | The generation data group of the data set.
datasetDetailOrgAttributes_gdg :: Lens.Lens' DatasetDetailOrgAttributes (Prelude.Maybe GdgDetailAttributes)
datasetDetailOrgAttributes_gdg = Lens.lens (\DatasetDetailOrgAttributes' {gdg} -> gdg) (\s@DatasetDetailOrgAttributes' {} a -> s {gdg = a} :: DatasetDetailOrgAttributes)

-- | The details of a VSAM data set.
datasetDetailOrgAttributes_vsam :: Lens.Lens' DatasetDetailOrgAttributes (Prelude.Maybe VsamDetailAttributes)
datasetDetailOrgAttributes_vsam = Lens.lens (\DatasetDetailOrgAttributes' {vsam} -> vsam) (\s@DatasetDetailOrgAttributes' {} a -> s {vsam = a} :: DatasetDetailOrgAttributes)

instance Data.FromJSON DatasetDetailOrgAttributes where
  parseJSON =
    Data.withObject
      "DatasetDetailOrgAttributes"
      ( \x ->
          DatasetDetailOrgAttributes'
            Prelude.<$> (x Data..:? "gdg")
            Prelude.<*> (x Data..:? "vsam")
      )

instance Prelude.Hashable DatasetDetailOrgAttributes where
  hashWithSalt _salt DatasetDetailOrgAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` gdg
      `Prelude.hashWithSalt` vsam

instance Prelude.NFData DatasetDetailOrgAttributes where
  rnf DatasetDetailOrgAttributes' {..} =
    Prelude.rnf gdg `Prelude.seq` Prelude.rnf vsam
