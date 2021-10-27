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
-- Module      : Network.AWS.Outposts.Types.Outpost
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Outposts.Types.Outpost where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an Outpost.
--
-- /See:/ 'newOutpost' smart constructor.
data Outpost = Outpost'
  { availabilityZoneId :: Prelude.Maybe Prelude.Text,
    outpostArn :: Prelude.Maybe Prelude.Text,
    ownerId :: Prelude.Maybe Prelude.Text,
    availabilityZone :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    lifeCycleStatus :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Outpost.
    outpostId :: Prelude.Maybe Prelude.Text,
    siteId :: Prelude.Maybe Prelude.Text,
    siteArn :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    -- | The Outpost tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Outpost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneId', 'outpost_availabilityZoneId' - Undocumented member.
--
-- 'outpostArn', 'outpost_outpostArn' - Undocumented member.
--
-- 'ownerId', 'outpost_ownerId' - Undocumented member.
--
-- 'availabilityZone', 'outpost_availabilityZone' - Undocumented member.
--
-- 'name', 'outpost_name' - Undocumented member.
--
-- 'lifeCycleStatus', 'outpost_lifeCycleStatus' - Undocumented member.
--
-- 'outpostId', 'outpost_outpostId' - The ID of the Outpost.
--
-- 'siteId', 'outpost_siteId' - Undocumented member.
--
-- 'siteArn', 'outpost_siteArn' - Undocumented member.
--
-- 'description', 'outpost_description' - Undocumented member.
--
-- 'tags', 'outpost_tags' - The Outpost tags.
newOutpost ::
  Outpost
newOutpost =
  Outpost'
    { availabilityZoneId = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      name = Prelude.Nothing,
      lifeCycleStatus = Prelude.Nothing,
      outpostId = Prelude.Nothing,
      siteId = Prelude.Nothing,
      siteArn = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Undocumented member.
outpost_availabilityZoneId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_availabilityZoneId = Lens.lens (\Outpost' {availabilityZoneId} -> availabilityZoneId) (\s@Outpost' {} a -> s {availabilityZoneId = a} :: Outpost)

-- | Undocumented member.
outpost_outpostArn :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_outpostArn = Lens.lens (\Outpost' {outpostArn} -> outpostArn) (\s@Outpost' {} a -> s {outpostArn = a} :: Outpost)

-- | Undocumented member.
outpost_ownerId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_ownerId = Lens.lens (\Outpost' {ownerId} -> ownerId) (\s@Outpost' {} a -> s {ownerId = a} :: Outpost)

-- | Undocumented member.
outpost_availabilityZone :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_availabilityZone = Lens.lens (\Outpost' {availabilityZone} -> availabilityZone) (\s@Outpost' {} a -> s {availabilityZone = a} :: Outpost)

-- | Undocumented member.
outpost_name :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_name = Lens.lens (\Outpost' {name} -> name) (\s@Outpost' {} a -> s {name = a} :: Outpost)

-- | Undocumented member.
outpost_lifeCycleStatus :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_lifeCycleStatus = Lens.lens (\Outpost' {lifeCycleStatus} -> lifeCycleStatus) (\s@Outpost' {} a -> s {lifeCycleStatus = a} :: Outpost)

-- | The ID of the Outpost.
outpost_outpostId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_outpostId = Lens.lens (\Outpost' {outpostId} -> outpostId) (\s@Outpost' {} a -> s {outpostId = a} :: Outpost)

-- | Undocumented member.
outpost_siteId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_siteId = Lens.lens (\Outpost' {siteId} -> siteId) (\s@Outpost' {} a -> s {siteId = a} :: Outpost)

-- | Undocumented member.
outpost_siteArn :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_siteArn = Lens.lens (\Outpost' {siteArn} -> siteArn) (\s@Outpost' {} a -> s {siteArn = a} :: Outpost)

-- | Undocumented member.
outpost_description :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_description = Lens.lens (\Outpost' {description} -> description) (\s@Outpost' {} a -> s {description = a} :: Outpost)

-- | The Outpost tags.
outpost_tags :: Lens.Lens' Outpost (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
outpost_tags = Lens.lens (\Outpost' {tags} -> tags) (\s@Outpost' {} a -> s {tags = a} :: Outpost) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Outpost where
  parseJSON =
    Core.withObject
      "Outpost"
      ( \x ->
          Outpost'
            Prelude.<$> (x Core..:? "AvailabilityZoneId")
            Prelude.<*> (x Core..:? "OutpostArn")
            Prelude.<*> (x Core..:? "OwnerId")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LifeCycleStatus")
            Prelude.<*> (x Core..:? "OutpostId")
            Prelude.<*> (x Core..:? "SiteId")
            Prelude.<*> (x Core..:? "SiteArn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Outpost

instance Prelude.NFData Outpost
