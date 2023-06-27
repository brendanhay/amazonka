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
-- Module      : Amazonka.Discovery.Types.ExportPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ExportPreferences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types.Ec2RecommendationsExportPreferences
import qualified Amazonka.Prelude as Prelude

-- | Indicates the type of data that is being exported. Only one
-- @ExportPreferences@ can be enabled for a
-- <https://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html StartExportTask>
-- action.
--
-- /See:/ 'newExportPreferences' smart constructor.
data ExportPreferences = ExportPreferences'
  { -- | If enabled, exported data includes EC2 instance type matches for
    -- on-premises servers discovered through Amazon Web Services Application
    -- Discovery Service.
    ec2RecommendationsPreferences :: Prelude.Maybe Ec2RecommendationsExportPreferences
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2RecommendationsPreferences', 'exportPreferences_ec2RecommendationsPreferences' - If enabled, exported data includes EC2 instance type matches for
-- on-premises servers discovered through Amazon Web Services Application
-- Discovery Service.
newExportPreferences ::
  ExportPreferences
newExportPreferences =
  ExportPreferences'
    { ec2RecommendationsPreferences =
        Prelude.Nothing
    }

-- | If enabled, exported data includes EC2 instance type matches for
-- on-premises servers discovered through Amazon Web Services Application
-- Discovery Service.
exportPreferences_ec2RecommendationsPreferences :: Lens.Lens' ExportPreferences (Prelude.Maybe Ec2RecommendationsExportPreferences)
exportPreferences_ec2RecommendationsPreferences = Lens.lens (\ExportPreferences' {ec2RecommendationsPreferences} -> ec2RecommendationsPreferences) (\s@ExportPreferences' {} a -> s {ec2RecommendationsPreferences = a} :: ExportPreferences)

instance Prelude.Hashable ExportPreferences where
  hashWithSalt _salt ExportPreferences' {..} =
    _salt
      `Prelude.hashWithSalt` ec2RecommendationsPreferences

instance Prelude.NFData ExportPreferences where
  rnf ExportPreferences' {..} =
    Prelude.rnf ec2RecommendationsPreferences

instance Data.ToJSON ExportPreferences where
  toJSON ExportPreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ec2RecommendationsPreferences" Data..=)
              Prelude.<$> ec2RecommendationsPreferences
          ]
      )
