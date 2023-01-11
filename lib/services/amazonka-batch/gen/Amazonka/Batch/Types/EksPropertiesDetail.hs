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
-- Module      : Amazonka.Batch.Types.EksPropertiesDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksPropertiesDetail where

import Amazonka.Batch.Types.EksPodPropertiesDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the details for the Kubernetes resources of a
-- job.
--
-- /See:/ 'newEksPropertiesDetail' smart constructor.
data EksPropertiesDetail = EksPropertiesDetail'
  { -- | The properties for the Kubernetes pod resources of a job.
    podProperties :: Prelude.Maybe EksPodPropertiesDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksPropertiesDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'podProperties', 'eksPropertiesDetail_podProperties' - The properties for the Kubernetes pod resources of a job.
newEksPropertiesDetail ::
  EksPropertiesDetail
newEksPropertiesDetail =
  EksPropertiesDetail'
    { podProperties =
        Prelude.Nothing
    }

-- | The properties for the Kubernetes pod resources of a job.
eksPropertiesDetail_podProperties :: Lens.Lens' EksPropertiesDetail (Prelude.Maybe EksPodPropertiesDetail)
eksPropertiesDetail_podProperties = Lens.lens (\EksPropertiesDetail' {podProperties} -> podProperties) (\s@EksPropertiesDetail' {} a -> s {podProperties = a} :: EksPropertiesDetail)

instance Data.FromJSON EksPropertiesDetail where
  parseJSON =
    Data.withObject
      "EksPropertiesDetail"
      ( \x ->
          EksPropertiesDetail'
            Prelude.<$> (x Data..:? "podProperties")
      )

instance Prelude.Hashable EksPropertiesDetail where
  hashWithSalt _salt EksPropertiesDetail' {..} =
    _salt `Prelude.hashWithSalt` podProperties

instance Prelude.NFData EksPropertiesDetail where
  rnf EksPropertiesDetail' {..} =
    Prelude.rnf podProperties
