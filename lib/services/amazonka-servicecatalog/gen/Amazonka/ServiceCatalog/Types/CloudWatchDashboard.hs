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
-- Module      : Amazonka.ServiceCatalog.Types.CloudWatchDashboard
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.CloudWatchDashboard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a CloudWatch dashboard.
--
-- /See:/ 'newCloudWatchDashboard' smart constructor.
data CloudWatchDashboard = CloudWatchDashboard'
  { -- | The name of the CloudWatch dashboard.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'cloudWatchDashboard_name' - The name of the CloudWatch dashboard.
newCloudWatchDashboard ::
  CloudWatchDashboard
newCloudWatchDashboard =
  CloudWatchDashboard' {name = Prelude.Nothing}

-- | The name of the CloudWatch dashboard.
cloudWatchDashboard_name :: Lens.Lens' CloudWatchDashboard (Prelude.Maybe Prelude.Text)
cloudWatchDashboard_name = Lens.lens (\CloudWatchDashboard' {name} -> name) (\s@CloudWatchDashboard' {} a -> s {name = a} :: CloudWatchDashboard)

instance Data.FromJSON CloudWatchDashboard where
  parseJSON =
    Data.withObject
      "CloudWatchDashboard"
      ( \x ->
          CloudWatchDashboard' Prelude.<$> (x Data..:? "Name")
      )

instance Prelude.Hashable CloudWatchDashboard where
  hashWithSalt _salt CloudWatchDashboard' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData CloudWatchDashboard where
  rnf CloudWatchDashboard' {..} = Prelude.rnf name
