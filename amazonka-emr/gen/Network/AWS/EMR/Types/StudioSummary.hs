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
-- Module      : Network.AWS.EMR.Types.StudioSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StudioSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details for an Amazon EMR Studio, including ID, Name, VPC, and
-- Description. The details do not include subnets, IAM roles, security
-- groups, or tags associated with the Studio.
--
-- /See:/ 'newStudioSummary' smart constructor.
data StudioSummary = StudioSummary'
  { -- | The time when the Amazon EMR Studio was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the Amazon EMR Studio.
    name :: Prelude.Maybe Prelude.Text,
    -- | The detailed description of the Amazon EMR Studio.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique access URL of the Amazon EMR Studio.
    url :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Virtual Private Cloud (Amazon VPC) associated with the
    -- Amazon EMR Studio.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { creationTime = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      url = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      studioId = Prelude.Nothing
    }

-- | The time when the Amazon EMR Studio was created.
studioSummary_creationTime :: Lens.Lens' StudioSummary (Prelude.Maybe Prelude.UTCTime)
studioSummary_creationTime = Lens.lens (\StudioSummary' {creationTime} -> creationTime) (\s@StudioSummary' {} a -> s {creationTime = a} :: StudioSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the Amazon EMR Studio.
studioSummary_name :: Lens.Lens' StudioSummary (Prelude.Maybe Prelude.Text)
studioSummary_name = Lens.lens (\StudioSummary' {name} -> name) (\s@StudioSummary' {} a -> s {name = a} :: StudioSummary)

-- | The detailed description of the Amazon EMR Studio.
studioSummary_description :: Lens.Lens' StudioSummary (Prelude.Maybe Prelude.Text)
studioSummary_description = Lens.lens (\StudioSummary' {description} -> description) (\s@StudioSummary' {} a -> s {description = a} :: StudioSummary)

-- | The unique access URL of the Amazon EMR Studio.
studioSummary_url :: Lens.Lens' StudioSummary (Prelude.Maybe Prelude.Text)
studioSummary_url = Lens.lens (\StudioSummary' {url} -> url) (\s@StudioSummary' {} a -> s {url = a} :: StudioSummary)

-- | The ID of the Virtual Private Cloud (Amazon VPC) associated with the
-- Amazon EMR Studio.
studioSummary_vpcId :: Lens.Lens' StudioSummary (Prelude.Maybe Prelude.Text)
studioSummary_vpcId = Lens.lens (\StudioSummary' {vpcId} -> vpcId) (\s@StudioSummary' {} a -> s {vpcId = a} :: StudioSummary)

-- | The ID of the Amazon EMR Studio.
studioSummary_studioId :: Lens.Lens' StudioSummary (Prelude.Maybe Prelude.Text)
studioSummary_studioId = Lens.lens (\StudioSummary' {studioId} -> studioId) (\s@StudioSummary' {} a -> s {studioId = a} :: StudioSummary)

instance Prelude.FromJSON StudioSummary where
  parseJSON =
    Prelude.withObject
      "StudioSummary"
      ( \x ->
          StudioSummary'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "Url")
            Prelude.<*> (x Prelude..:? "VpcId")
            Prelude.<*> (x Prelude..:? "StudioId")
      )

instance Prelude.Hashable StudioSummary

instance Prelude.NFData StudioSummary
