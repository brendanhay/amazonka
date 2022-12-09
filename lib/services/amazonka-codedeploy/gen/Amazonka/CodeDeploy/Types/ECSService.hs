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
-- Module      : Amazonka.CodeDeploy.Types.ECSService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.ECSService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the service and cluster names used to identify an Amazon ECS
-- deployment\'s target.
--
-- /See:/ 'newECSService' smart constructor.
data ECSService = ECSService'
  { -- | The name of the cluster that the Amazon ECS service is associated with.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The name of the target Amazon ECS service.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'eCSService_clusterName' - The name of the cluster that the Amazon ECS service is associated with.
--
-- 'serviceName', 'eCSService_serviceName' - The name of the target Amazon ECS service.
newECSService ::
  ECSService
newECSService =
  ECSService'
    { clusterName = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | The name of the cluster that the Amazon ECS service is associated with.
eCSService_clusterName :: Lens.Lens' ECSService (Prelude.Maybe Prelude.Text)
eCSService_clusterName = Lens.lens (\ECSService' {clusterName} -> clusterName) (\s@ECSService' {} a -> s {clusterName = a} :: ECSService)

-- | The name of the target Amazon ECS service.
eCSService_serviceName :: Lens.Lens' ECSService (Prelude.Maybe Prelude.Text)
eCSService_serviceName = Lens.lens (\ECSService' {serviceName} -> serviceName) (\s@ECSService' {} a -> s {serviceName = a} :: ECSService)

instance Data.FromJSON ECSService where
  parseJSON =
    Data.withObject
      "ECSService"
      ( \x ->
          ECSService'
            Prelude.<$> (x Data..:? "clusterName")
            Prelude.<*> (x Data..:? "serviceName")
      )

instance Prelude.Hashable ECSService where
  hashWithSalt _salt ECSService' {..} =
    _salt `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ECSService where
  rnf ECSService' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToJSON ECSService where
  toJSON ECSService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clusterName" Data..=) Prelude.<$> clusterName,
            ("serviceName" Data..=) Prelude.<$> serviceName
          ]
      )
