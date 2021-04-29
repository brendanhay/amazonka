{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.Types.BulkDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeployment where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a bulk deployment. You cannot start a new bulk
-- deployment while another one is still running or in a non-terminal
-- state.
--
-- /See:/ 'newBulkDeployment' smart constructor.
data BulkDeployment = BulkDeployment'
  { -- | The ID of the bulk deployment.
    bulkDeploymentId :: Prelude.Maybe Prelude.Text,
    -- | The time, in ISO format, when the deployment was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the bulk deployment.
    bulkDeploymentArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BulkDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bulkDeploymentId', 'bulkDeployment_bulkDeploymentId' - The ID of the bulk deployment.
--
-- 'createdAt', 'bulkDeployment_createdAt' - The time, in ISO format, when the deployment was created.
--
-- 'bulkDeploymentArn', 'bulkDeployment_bulkDeploymentArn' - The ARN of the bulk deployment.
newBulkDeployment ::
  BulkDeployment
newBulkDeployment =
  BulkDeployment'
    { bulkDeploymentId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      bulkDeploymentArn = Prelude.Nothing
    }

-- | The ID of the bulk deployment.
bulkDeployment_bulkDeploymentId :: Lens.Lens' BulkDeployment (Prelude.Maybe Prelude.Text)
bulkDeployment_bulkDeploymentId = Lens.lens (\BulkDeployment' {bulkDeploymentId} -> bulkDeploymentId) (\s@BulkDeployment' {} a -> s {bulkDeploymentId = a} :: BulkDeployment)

-- | The time, in ISO format, when the deployment was created.
bulkDeployment_createdAt :: Lens.Lens' BulkDeployment (Prelude.Maybe Prelude.Text)
bulkDeployment_createdAt = Lens.lens (\BulkDeployment' {createdAt} -> createdAt) (\s@BulkDeployment' {} a -> s {createdAt = a} :: BulkDeployment)

-- | The ARN of the bulk deployment.
bulkDeployment_bulkDeploymentArn :: Lens.Lens' BulkDeployment (Prelude.Maybe Prelude.Text)
bulkDeployment_bulkDeploymentArn = Lens.lens (\BulkDeployment' {bulkDeploymentArn} -> bulkDeploymentArn) (\s@BulkDeployment' {} a -> s {bulkDeploymentArn = a} :: BulkDeployment)

instance Prelude.FromJSON BulkDeployment where
  parseJSON =
    Prelude.withObject
      "BulkDeployment"
      ( \x ->
          BulkDeployment'
            Prelude.<$> (x Prelude..:? "BulkDeploymentId")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "BulkDeploymentArn")
      )

instance Prelude.Hashable BulkDeployment

instance Prelude.NFData BulkDeployment
