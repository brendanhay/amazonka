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
-- Module      : Amazonka.EC2.Types.ElasticGpuHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ElasticGpuHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ElasticGpuStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of an Elastic Graphics accelerator.
--
-- /See:/ 'newElasticGpuHealth' smart constructor.
data ElasticGpuHealth = ElasticGpuHealth'
  { -- | The health status.
    status :: Prelude.Maybe ElasticGpuStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticGpuHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'elasticGpuHealth_status' - The health status.
newElasticGpuHealth ::
  ElasticGpuHealth
newElasticGpuHealth =
  ElasticGpuHealth' {status = Prelude.Nothing}

-- | The health status.
elasticGpuHealth_status :: Lens.Lens' ElasticGpuHealth (Prelude.Maybe ElasticGpuStatus)
elasticGpuHealth_status = Lens.lens (\ElasticGpuHealth' {status} -> status) (\s@ElasticGpuHealth' {} a -> s {status = a} :: ElasticGpuHealth)

instance Data.FromXML ElasticGpuHealth where
  parseXML x =
    ElasticGpuHealth' Prelude.<$> (x Data..@? "status")

instance Prelude.Hashable ElasticGpuHealth where
  hashWithSalt _salt ElasticGpuHealth' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData ElasticGpuHealth where
  rnf ElasticGpuHealth' {..} = Prelude.rnf status
