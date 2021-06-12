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
-- Module      : Network.AWS.EMR.Types.StudioSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StudioSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details for an Amazon EMR Studio, including ID, Name, VPC, and
-- Description. The details do not include subnets, IAM roles, security
-- groups, or tags associated with the Studio.
--
-- /See:/ 'newStudioSummary' smart constructor.
data StudioSummary = StudioSummary'
  { -- | The time when the Amazon EMR Studio was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The name of the Amazon EMR Studio.
    name :: Core.Maybe Core.Text,
    -- | The detailed description of the Amazon EMR Studio.
    description :: Core.Maybe Core.Text,
    -- | The unique access URL of the Amazon EMR Studio.
    url :: Core.Maybe Core.Text,
    -- | The ID of the Virtual Private Cloud (Amazon VPC) associated with the
    -- Amazon EMR Studio.
    vpcId :: Core.Maybe Core.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StudioSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'studioSummary_creationTime' - The time when the Amazon EMR Studio was created.
--
-- 'name', 'studioSummary_name' - The name of the Amazon EMR Studio.
--
-- 'description', 'studioSummary_description' - The detailed description of the Amazon EMR Studio.
--
-- 'url', 'studioSummary_url' - The unique access URL of the Amazon EMR Studio.
--
-- 'vpcId', 'studioSummary_vpcId' - The ID of the Virtual Private Cloud (Amazon VPC) associated with the
-- Amazon EMR Studio.
--
-- 'studioId', 'studioSummary_studioId' - The ID of the Amazon EMR Studio.
newStudioSummary ::
  StudioSummary
newStudioSummary =
  StudioSummary'
    { creationTime = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      url = Core.Nothing,
      vpcId = Core.Nothing,
      studioId = Core.Nothing
    }

-- | The time when the Amazon EMR Studio was created.
studioSummary_creationTime :: Lens.Lens' StudioSummary (Core.Maybe Core.UTCTime)
studioSummary_creationTime = Lens.lens (\StudioSummary' {creationTime} -> creationTime) (\s@StudioSummary' {} a -> s {creationTime = a} :: StudioSummary) Core.. Lens.mapping Core._Time

-- | The name of the Amazon EMR Studio.
studioSummary_name :: Lens.Lens' StudioSummary (Core.Maybe Core.Text)
studioSummary_name = Lens.lens (\StudioSummary' {name} -> name) (\s@StudioSummary' {} a -> s {name = a} :: StudioSummary)

-- | The detailed description of the Amazon EMR Studio.
studioSummary_description :: Lens.Lens' StudioSummary (Core.Maybe Core.Text)
studioSummary_description = Lens.lens (\StudioSummary' {description} -> description) (\s@StudioSummary' {} a -> s {description = a} :: StudioSummary)

-- | The unique access URL of the Amazon EMR Studio.
studioSummary_url :: Lens.Lens' StudioSummary (Core.Maybe Core.Text)
studioSummary_url = Lens.lens (\StudioSummary' {url} -> url) (\s@StudioSummary' {} a -> s {url = a} :: StudioSummary)

-- | The ID of the Virtual Private Cloud (Amazon VPC) associated with the
-- Amazon EMR Studio.
studioSummary_vpcId :: Lens.Lens' StudioSummary (Core.Maybe Core.Text)
studioSummary_vpcId = Lens.lens (\StudioSummary' {vpcId} -> vpcId) (\s@StudioSummary' {} a -> s {vpcId = a} :: StudioSummary)

-- | The ID of the Amazon EMR Studio.
studioSummary_studioId :: Lens.Lens' StudioSummary (Core.Maybe Core.Text)
studioSummary_studioId = Lens.lens (\StudioSummary' {studioId} -> studioId) (\s@StudioSummary' {} a -> s {studioId = a} :: StudioSummary)

instance Core.FromJSON StudioSummary where
  parseJSON =
    Core.withObject
      "StudioSummary"
      ( \x ->
          StudioSummary'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Url")
            Core.<*> (x Core..:? "VpcId")
            Core.<*> (x Core..:? "StudioId")
      )

instance Core.Hashable StudioSummary

instance Core.NFData StudioSummary
