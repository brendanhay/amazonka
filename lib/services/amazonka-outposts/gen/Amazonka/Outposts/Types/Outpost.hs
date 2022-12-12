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
-- Module      : Amazonka.Outposts.Types.Outpost
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.Outpost where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.SupportedHardwareType
import qualified Amazonka.Prelude as Prelude

-- | Information about an Outpost.
--
-- /See:/ 'newOutpost' smart constructor.
data Outpost = Outpost'
  { availabilityZone :: Prelude.Maybe Prelude.Text,
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    lifeCycleStatus :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Outpost.
    outpostId :: Prelude.Maybe Prelude.Text,
    ownerId :: Prelude.Maybe Prelude.Text,
    siteArn :: Prelude.Maybe Prelude.Text,
    siteId :: Prelude.Maybe Prelude.Text,
    -- | The hardware type.
    supportedHardwareType :: Prelude.Maybe SupportedHardwareType,
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
-- 'availabilityZone', 'outpost_availabilityZone' - Undocumented member.
--
-- 'availabilityZoneId', 'outpost_availabilityZoneId' - Undocumented member.
--
-- 'description', 'outpost_description' - Undocumented member.
--
-- 'lifeCycleStatus', 'outpost_lifeCycleStatus' - Undocumented member.
--
-- 'name', 'outpost_name' - Undocumented member.
--
-- 'outpostArn', 'outpost_outpostArn' - Undocumented member.
--
-- 'outpostId', 'outpost_outpostId' - The ID of the Outpost.
--
-- 'ownerId', 'outpost_ownerId' - Undocumented member.
--
-- 'siteArn', 'outpost_siteArn' - Undocumented member.
--
-- 'siteId', 'outpost_siteId' - Undocumented member.
--
-- 'supportedHardwareType', 'outpost_supportedHardwareType' - The hardware type.
--
-- 'tags', 'outpost_tags' - The Outpost tags.
newOutpost ::
  Outpost
newOutpost =
  Outpost'
    { availabilityZone = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      description = Prelude.Nothing,
      lifeCycleStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      outpostId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      siteArn = Prelude.Nothing,
      siteId = Prelude.Nothing,
      supportedHardwareType = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Undocumented member.
outpost_availabilityZone :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_availabilityZone = Lens.lens (\Outpost' {availabilityZone} -> availabilityZone) (\s@Outpost' {} a -> s {availabilityZone = a} :: Outpost)

-- | Undocumented member.
outpost_availabilityZoneId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_availabilityZoneId = Lens.lens (\Outpost' {availabilityZoneId} -> availabilityZoneId) (\s@Outpost' {} a -> s {availabilityZoneId = a} :: Outpost)

-- | Undocumented member.
outpost_description :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_description = Lens.lens (\Outpost' {description} -> description) (\s@Outpost' {} a -> s {description = a} :: Outpost)

-- | Undocumented member.
outpost_lifeCycleStatus :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_lifeCycleStatus = Lens.lens (\Outpost' {lifeCycleStatus} -> lifeCycleStatus) (\s@Outpost' {} a -> s {lifeCycleStatus = a} :: Outpost)

-- | Undocumented member.
outpost_name :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_name = Lens.lens (\Outpost' {name} -> name) (\s@Outpost' {} a -> s {name = a} :: Outpost)

-- | Undocumented member.
outpost_outpostArn :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_outpostArn = Lens.lens (\Outpost' {outpostArn} -> outpostArn) (\s@Outpost' {} a -> s {outpostArn = a} :: Outpost)

-- | The ID of the Outpost.
outpost_outpostId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_outpostId = Lens.lens (\Outpost' {outpostId} -> outpostId) (\s@Outpost' {} a -> s {outpostId = a} :: Outpost)

-- | Undocumented member.
outpost_ownerId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_ownerId = Lens.lens (\Outpost' {ownerId} -> ownerId) (\s@Outpost' {} a -> s {ownerId = a} :: Outpost)

-- | Undocumented member.
outpost_siteArn :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_siteArn = Lens.lens (\Outpost' {siteArn} -> siteArn) (\s@Outpost' {} a -> s {siteArn = a} :: Outpost)

-- | Undocumented member.
outpost_siteId :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_siteId = Lens.lens (\Outpost' {siteId} -> siteId) (\s@Outpost' {} a -> s {siteId = a} :: Outpost)

-- | The hardware type.
outpost_supportedHardwareType :: Lens.Lens' Outpost (Prelude.Maybe SupportedHardwareType)
outpost_supportedHardwareType = Lens.lens (\Outpost' {supportedHardwareType} -> supportedHardwareType) (\s@Outpost' {} a -> s {supportedHardwareType = a} :: Outpost)

-- | The Outpost tags.
outpost_tags :: Lens.Lens' Outpost (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
outpost_tags = Lens.lens (\Outpost' {tags} -> tags) (\s@Outpost' {} a -> s {tags = a} :: Outpost) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Outpost where
  parseJSON =
    Data.withObject
      "Outpost"
      ( \x ->
          Outpost'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "AvailabilityZoneId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LifeCycleStatus")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OutpostArn")
            Prelude.<*> (x Data..:? "OutpostId")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "SiteArn")
            Prelude.<*> (x Data..:? "SiteId")
            Prelude.<*> (x Data..:? "SupportedHardwareType")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Outpost where
  hashWithSalt _salt Outpost' {..} =
    _salt `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lifeCycleStatus
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` outpostId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` siteArn
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` supportedHardwareType
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Outpost where
  rnf Outpost' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lifeCycleStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf outpostId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf siteArn
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf supportedHardwareType
      `Prelude.seq` Prelude.rnf tags
