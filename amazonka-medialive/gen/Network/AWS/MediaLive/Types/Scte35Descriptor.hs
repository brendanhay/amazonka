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
-- Module      : Network.AWS.MediaLive.Types.Scte35Descriptor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35Descriptor where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35DescriptorSettings
import qualified Network.AWS.Prelude as Prelude

-- | Holds one set of SCTE-35 Descriptor Settings.
--
-- /See:/ 'newScte35Descriptor' smart constructor.
data Scte35Descriptor = Scte35Descriptor'
  { -- | SCTE-35 Descriptor Settings.
    scte35DescriptorSettings :: Scte35DescriptorSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Scte35Descriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scte35DescriptorSettings', 'scte35Descriptor_scte35DescriptorSettings' - SCTE-35 Descriptor Settings.
newScte35Descriptor ::
  -- | 'scte35DescriptorSettings'
  Scte35DescriptorSettings ->
  Scte35Descriptor
newScte35Descriptor pScte35DescriptorSettings_ =
  Scte35Descriptor'
    { scte35DescriptorSettings =
        pScte35DescriptorSettings_
    }

-- | SCTE-35 Descriptor Settings.
scte35Descriptor_scte35DescriptorSettings :: Lens.Lens' Scte35Descriptor Scte35DescriptorSettings
scte35Descriptor_scte35DescriptorSettings = Lens.lens (\Scte35Descriptor' {scte35DescriptorSettings} -> scte35DescriptorSettings) (\s@Scte35Descriptor' {} a -> s {scte35DescriptorSettings = a} :: Scte35Descriptor)

instance Prelude.FromJSON Scte35Descriptor where
  parseJSON =
    Prelude.withObject
      "Scte35Descriptor"
      ( \x ->
          Scte35Descriptor'
            Prelude.<$> (x Prelude..: "scte35DescriptorSettings")
      )

instance Prelude.Hashable Scte35Descriptor

instance Prelude.NFData Scte35Descriptor

instance Prelude.ToJSON Scte35Descriptor where
  toJSON Scte35Descriptor' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "scte35DescriptorSettings"
                  Prelude..= scte35DescriptorSettings
              )
          ]
      )
