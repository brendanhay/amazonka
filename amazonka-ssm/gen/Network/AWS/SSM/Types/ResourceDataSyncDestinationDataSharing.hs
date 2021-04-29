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
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Synchronize Systems Manager Inventory data from multiple AWS accounts
-- defined in AWS Organizations to a centralized S3 bucket. Data is
-- synchronized to individual key prefixes in the central bucket. Each key
-- prefix represents a different AWS account ID.
--
-- /See:/ 'newResourceDataSyncDestinationDataSharing' smart constructor.
data ResourceDataSyncDestinationDataSharing = ResourceDataSyncDestinationDataSharing'
  { -- | The sharing data type. Only @Organization@ is supported.
    destinationDataSharingType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncDestinationDataSharing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationDataSharingType', 'resourceDataSyncDestinationDataSharing_destinationDataSharingType' - The sharing data type. Only @Organization@ is supported.
newResourceDataSyncDestinationDataSharing ::
  ResourceDataSyncDestinationDataSharing
newResourceDataSyncDestinationDataSharing =
  ResourceDataSyncDestinationDataSharing'
    { destinationDataSharingType =
        Prelude.Nothing
    }

-- | The sharing data type. Only @Organization@ is supported.
resourceDataSyncDestinationDataSharing_destinationDataSharingType :: Lens.Lens' ResourceDataSyncDestinationDataSharing (Prelude.Maybe Prelude.Text)
resourceDataSyncDestinationDataSharing_destinationDataSharingType = Lens.lens (\ResourceDataSyncDestinationDataSharing' {destinationDataSharingType} -> destinationDataSharingType) (\s@ResourceDataSyncDestinationDataSharing' {} a -> s {destinationDataSharingType = a} :: ResourceDataSyncDestinationDataSharing)

instance
  Prelude.FromJSON
    ResourceDataSyncDestinationDataSharing
  where
  parseJSON =
    Prelude.withObject
      "ResourceDataSyncDestinationDataSharing"
      ( \x ->
          ResourceDataSyncDestinationDataSharing'
            Prelude.<$> (x Prelude..:? "DestinationDataSharingType")
      )

instance
  Prelude.Hashable
    ResourceDataSyncDestinationDataSharing

instance
  Prelude.NFData
    ResourceDataSyncDestinationDataSharing

instance
  Prelude.ToJSON
    ResourceDataSyncDestinationDataSharing
  where
  toJSON ResourceDataSyncDestinationDataSharing' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DestinationDataSharingType" Prelude..=)
              Prelude.<$> destinationDataSharingType
          ]
      )
