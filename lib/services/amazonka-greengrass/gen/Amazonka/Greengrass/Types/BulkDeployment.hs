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
-- Module      : Amazonka.Greengrass.Types.BulkDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.BulkDeployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a bulk deployment. You cannot start a new bulk
-- deployment while another one is still running or in a non-terminal
-- state.
--
-- /See:/ 'newBulkDeployment' smart constructor.
data BulkDeployment = BulkDeployment'
  { -- | The ARN of the bulk deployment.
    bulkDeploymentArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the bulk deployment.
    bulkDeploymentId :: Prelude.Maybe Prelude.Text,
    -- | The time, in ISO format, when the deployment was created.
    createdAt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BulkDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bulkDeploymentArn', 'bulkDeployment_bulkDeploymentArn' - The ARN of the bulk deployment.
--
-- 'bulkDeploymentId', 'bulkDeployment_bulkDeploymentId' - The ID of the bulk deployment.
--
-- 'createdAt', 'bulkDeployment_createdAt' - The time, in ISO format, when the deployment was created.
newBulkDeployment ::
  BulkDeployment
newBulkDeployment =
  BulkDeployment'
    { bulkDeploymentArn =
        Prelude.Nothing,
      bulkDeploymentId = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The ARN of the bulk deployment.
bulkDeployment_bulkDeploymentArn :: Lens.Lens' BulkDeployment (Prelude.Maybe Prelude.Text)
bulkDeployment_bulkDeploymentArn = Lens.lens (\BulkDeployment' {bulkDeploymentArn} -> bulkDeploymentArn) (\s@BulkDeployment' {} a -> s {bulkDeploymentArn = a} :: BulkDeployment)

-- | The ID of the bulk deployment.
bulkDeployment_bulkDeploymentId :: Lens.Lens' BulkDeployment (Prelude.Maybe Prelude.Text)
bulkDeployment_bulkDeploymentId = Lens.lens (\BulkDeployment' {bulkDeploymentId} -> bulkDeploymentId) (\s@BulkDeployment' {} a -> s {bulkDeploymentId = a} :: BulkDeployment)

-- | The time, in ISO format, when the deployment was created.
bulkDeployment_createdAt :: Lens.Lens' BulkDeployment (Prelude.Maybe Prelude.Text)
bulkDeployment_createdAt = Lens.lens (\BulkDeployment' {createdAt} -> createdAt) (\s@BulkDeployment' {} a -> s {createdAt = a} :: BulkDeployment)

instance Data.FromJSON BulkDeployment where
  parseJSON =
    Data.withObject
      "BulkDeployment"
      ( \x ->
          BulkDeployment'
            Prelude.<$> (x Data..:? "BulkDeploymentArn")
            Prelude.<*> (x Data..:? "BulkDeploymentId")
            Prelude.<*> (x Data..:? "CreatedAt")
      )

instance Prelude.Hashable BulkDeployment where
  hashWithSalt _salt BulkDeployment' {..} =
    _salt `Prelude.hashWithSalt` bulkDeploymentArn
      `Prelude.hashWithSalt` bulkDeploymentId
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData BulkDeployment where
  rnf BulkDeployment' {..} =
    Prelude.rnf bulkDeploymentArn
      `Prelude.seq` Prelude.rnf bulkDeploymentId
      `Prelude.seq` Prelude.rnf createdAt
