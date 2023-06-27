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
-- Module      : Amazonka.DataSync.Types.ResourceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.ResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.NetAppONTAPCluster
import Amazonka.DataSync.Types.NetAppONTAPSVM
import Amazonka.DataSync.Types.NetAppONTAPVolume
import qualified Amazonka.Prelude as Prelude

-- | Information provided by DataSync Discovery about the resources in your
-- on-premises storage system.
--
-- /See:/ 'newResourceDetails' smart constructor.
data ResourceDetails = ResourceDetails'
  { -- | The information that DataSync Discovery collects about the cluster in
    -- your on-premises storage system.
    netAppONTAPClusters :: Prelude.Maybe [NetAppONTAPCluster],
    -- | The information that DataSync Discovery collects about storage virtual
    -- machines (SVMs) in your on-premises storage system.
    netAppONTAPSVMs :: Prelude.Maybe [NetAppONTAPSVM],
    -- | The information that DataSync Discovery collects about volumes in your
    -- on-premises storage system.
    netAppONTAPVolumes :: Prelude.Maybe [NetAppONTAPVolume]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'netAppONTAPClusters', 'resourceDetails_netAppONTAPClusters' - The information that DataSync Discovery collects about the cluster in
-- your on-premises storage system.
--
-- 'netAppONTAPSVMs', 'resourceDetails_netAppONTAPSVMs' - The information that DataSync Discovery collects about storage virtual
-- machines (SVMs) in your on-premises storage system.
--
-- 'netAppONTAPVolumes', 'resourceDetails_netAppONTAPVolumes' - The information that DataSync Discovery collects about volumes in your
-- on-premises storage system.
newResourceDetails ::
  ResourceDetails
newResourceDetails =
  ResourceDetails'
    { netAppONTAPClusters =
        Prelude.Nothing,
      netAppONTAPSVMs = Prelude.Nothing,
      netAppONTAPVolumes = Prelude.Nothing
    }

-- | The information that DataSync Discovery collects about the cluster in
-- your on-premises storage system.
resourceDetails_netAppONTAPClusters :: Lens.Lens' ResourceDetails (Prelude.Maybe [NetAppONTAPCluster])
resourceDetails_netAppONTAPClusters = Lens.lens (\ResourceDetails' {netAppONTAPClusters} -> netAppONTAPClusters) (\s@ResourceDetails' {} a -> s {netAppONTAPClusters = a} :: ResourceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The information that DataSync Discovery collects about storage virtual
-- machines (SVMs) in your on-premises storage system.
resourceDetails_netAppONTAPSVMs :: Lens.Lens' ResourceDetails (Prelude.Maybe [NetAppONTAPSVM])
resourceDetails_netAppONTAPSVMs = Lens.lens (\ResourceDetails' {netAppONTAPSVMs} -> netAppONTAPSVMs) (\s@ResourceDetails' {} a -> s {netAppONTAPSVMs = a} :: ResourceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The information that DataSync Discovery collects about volumes in your
-- on-premises storage system.
resourceDetails_netAppONTAPVolumes :: Lens.Lens' ResourceDetails (Prelude.Maybe [NetAppONTAPVolume])
resourceDetails_netAppONTAPVolumes = Lens.lens (\ResourceDetails' {netAppONTAPVolumes} -> netAppONTAPVolumes) (\s@ResourceDetails' {} a -> s {netAppONTAPVolumes = a} :: ResourceDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceDetails where
  parseJSON =
    Data.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails'
            Prelude.<$> ( x
                            Data..:? "NetAppONTAPClusters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "NetAppONTAPSVMs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "NetAppONTAPVolumes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ResourceDetails where
  hashWithSalt _salt ResourceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` netAppONTAPClusters
      `Prelude.hashWithSalt` netAppONTAPSVMs
      `Prelude.hashWithSalt` netAppONTAPVolumes

instance Prelude.NFData ResourceDetails where
  rnf ResourceDetails' {..} =
    Prelude.rnf netAppONTAPClusters
      `Prelude.seq` Prelude.rnf netAppONTAPSVMs
      `Prelude.seq` Prelude.rnf netAppONTAPVolumes
