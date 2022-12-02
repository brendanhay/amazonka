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
-- Module      : Amazonka.NetworkManager.Types.LinkAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.LinkAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.LinkAssociationState
import qualified Amazonka.Prelude as Prelude

-- | Describes the association between a device and a link.
--
-- /See:/ 'newLinkAssociation' smart constructor.
data LinkAssociation = LinkAssociation'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the link.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The device ID for the link association.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The state of the association.
    linkAssociationState :: Prelude.Maybe LinkAssociationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LinkAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'linkAssociation_globalNetworkId' - The ID of the global network.
--
-- 'linkId', 'linkAssociation_linkId' - The ID of the link.
--
-- 'deviceId', 'linkAssociation_deviceId' - The device ID for the link association.
--
-- 'linkAssociationState', 'linkAssociation_linkAssociationState' - The state of the association.
newLinkAssociation ::
  LinkAssociation
newLinkAssociation =
  LinkAssociation'
    { globalNetworkId = Prelude.Nothing,
      linkId = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      linkAssociationState = Prelude.Nothing
    }

-- | The ID of the global network.
linkAssociation_globalNetworkId :: Lens.Lens' LinkAssociation (Prelude.Maybe Prelude.Text)
linkAssociation_globalNetworkId = Lens.lens (\LinkAssociation' {globalNetworkId} -> globalNetworkId) (\s@LinkAssociation' {} a -> s {globalNetworkId = a} :: LinkAssociation)

-- | The ID of the link.
linkAssociation_linkId :: Lens.Lens' LinkAssociation (Prelude.Maybe Prelude.Text)
linkAssociation_linkId = Lens.lens (\LinkAssociation' {linkId} -> linkId) (\s@LinkAssociation' {} a -> s {linkId = a} :: LinkAssociation)

-- | The device ID for the link association.
linkAssociation_deviceId :: Lens.Lens' LinkAssociation (Prelude.Maybe Prelude.Text)
linkAssociation_deviceId = Lens.lens (\LinkAssociation' {deviceId} -> deviceId) (\s@LinkAssociation' {} a -> s {deviceId = a} :: LinkAssociation)

-- | The state of the association.
linkAssociation_linkAssociationState :: Lens.Lens' LinkAssociation (Prelude.Maybe LinkAssociationState)
linkAssociation_linkAssociationState = Lens.lens (\LinkAssociation' {linkAssociationState} -> linkAssociationState) (\s@LinkAssociation' {} a -> s {linkAssociationState = a} :: LinkAssociation)

instance Data.FromJSON LinkAssociation where
  parseJSON =
    Data.withObject
      "LinkAssociation"
      ( \x ->
          LinkAssociation'
            Prelude.<$> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "LinkId")
            Prelude.<*> (x Data..:? "DeviceId")
            Prelude.<*> (x Data..:? "LinkAssociationState")
      )

instance Prelude.Hashable LinkAssociation where
  hashWithSalt _salt LinkAssociation' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` linkId
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` linkAssociationState

instance Prelude.NFData LinkAssociation where
  rnf LinkAssociation' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf linkAssociationState
