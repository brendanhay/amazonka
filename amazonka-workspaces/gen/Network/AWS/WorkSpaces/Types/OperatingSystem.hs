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
-- Module      : Network.AWS.WorkSpaces.Types.OperatingSystem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.OperatingSystem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.OperatingSystemType

-- | The operating system that the image is running.
--
-- /See:/ 'newOperatingSystem' smart constructor.
data OperatingSystem = OperatingSystem'
  { -- | The operating system.
    type' :: Prelude.Maybe OperatingSystemType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OperatingSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'operatingSystem_type' - The operating system.
newOperatingSystem ::
  OperatingSystem
newOperatingSystem =
  OperatingSystem' {type' = Prelude.Nothing}

-- | The operating system.
operatingSystem_type :: Lens.Lens' OperatingSystem (Prelude.Maybe OperatingSystemType)
operatingSystem_type = Lens.lens (\OperatingSystem' {type'} -> type') (\s@OperatingSystem' {} a -> s {type' = a} :: OperatingSystem)

instance Prelude.FromJSON OperatingSystem where
  parseJSON =
    Prelude.withObject
      "OperatingSystem"
      ( \x ->
          OperatingSystem' Prelude.<$> (x Prelude..:? "Type")
      )

instance Prelude.Hashable OperatingSystem

instance Prelude.NFData OperatingSystem
