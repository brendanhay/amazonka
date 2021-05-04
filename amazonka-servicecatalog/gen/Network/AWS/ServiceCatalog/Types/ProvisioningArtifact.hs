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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifact where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance

-- | Information about a provisioning artifact. A provisioning artifact is
-- also known as a product version.
--
-- /See:/ 'newProvisioningArtifact' smart constructor.
data ProvisioningArtifact = ProvisioningArtifact'
  { -- | Information set by the administrator to provide guidance to end users
    -- about which provisioning artifacts to use.
    guidance :: Prelude.Maybe ProvisioningArtifactGuidance,
    -- | The identifier of the provisioning artifact.
    id :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the provisioning artifact.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the provisioning artifact.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'guidance', 'provisioningArtifact_guidance' - Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
--
-- 'id', 'provisioningArtifact_id' - The identifier of the provisioning artifact.
--
-- 'createdTime', 'provisioningArtifact_createdTime' - The UTC time stamp of the creation time.
--
-- 'name', 'provisioningArtifact_name' - The name of the provisioning artifact.
--
-- 'description', 'provisioningArtifact_description' - The description of the provisioning artifact.
newProvisioningArtifact ::
  ProvisioningArtifact
newProvisioningArtifact =
  ProvisioningArtifact'
    { guidance = Prelude.Nothing,
      id = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
provisioningArtifact_guidance :: Lens.Lens' ProvisioningArtifact (Prelude.Maybe ProvisioningArtifactGuidance)
provisioningArtifact_guidance = Lens.lens (\ProvisioningArtifact' {guidance} -> guidance) (\s@ProvisioningArtifact' {} a -> s {guidance = a} :: ProvisioningArtifact)

-- | The identifier of the provisioning artifact.
provisioningArtifact_id :: Lens.Lens' ProvisioningArtifact (Prelude.Maybe Prelude.Text)
provisioningArtifact_id = Lens.lens (\ProvisioningArtifact' {id} -> id) (\s@ProvisioningArtifact' {} a -> s {id = a} :: ProvisioningArtifact)

-- | The UTC time stamp of the creation time.
provisioningArtifact_createdTime :: Lens.Lens' ProvisioningArtifact (Prelude.Maybe Prelude.UTCTime)
provisioningArtifact_createdTime = Lens.lens (\ProvisioningArtifact' {createdTime} -> createdTime) (\s@ProvisioningArtifact' {} a -> s {createdTime = a} :: ProvisioningArtifact) Prelude.. Lens.mapping Prelude._Time

-- | The name of the provisioning artifact.
provisioningArtifact_name :: Lens.Lens' ProvisioningArtifact (Prelude.Maybe Prelude.Text)
provisioningArtifact_name = Lens.lens (\ProvisioningArtifact' {name} -> name) (\s@ProvisioningArtifact' {} a -> s {name = a} :: ProvisioningArtifact)

-- | The description of the provisioning artifact.
provisioningArtifact_description :: Lens.Lens' ProvisioningArtifact (Prelude.Maybe Prelude.Text)
provisioningArtifact_description = Lens.lens (\ProvisioningArtifact' {description} -> description) (\s@ProvisioningArtifact' {} a -> s {description = a} :: ProvisioningArtifact)

instance Prelude.FromJSON ProvisioningArtifact where
  parseJSON =
    Prelude.withObject
      "ProvisioningArtifact"
      ( \x ->
          ProvisioningArtifact'
            Prelude.<$> (x Prelude..:? "Guidance")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance Prelude.Hashable ProvisioningArtifact

instance Prelude.NFData ProvisioningArtifact
