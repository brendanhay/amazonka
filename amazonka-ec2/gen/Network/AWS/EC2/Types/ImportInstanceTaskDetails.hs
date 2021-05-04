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
-- Module      : Network.AWS.EC2.Types.ImportInstanceTaskDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportInstanceTaskDetails where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ImportInstanceVolumeDetailItem
import Network.AWS.EC2.Types.PlatformValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an import instance task.
--
-- /See:/ 'newImportInstanceTaskDetails' smart constructor.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails'
  { -- | The instance operating system.
    platform :: Prelude.Maybe PlatformValues,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The volumes.
    volumes :: Prelude.Maybe [ImportInstanceVolumeDetailItem],
    -- | A description of the task.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportInstanceTaskDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'importInstanceTaskDetails_platform' - The instance operating system.
--
-- 'instanceId', 'importInstanceTaskDetails_instanceId' - The ID of the instance.
--
-- 'volumes', 'importInstanceTaskDetails_volumes' - The volumes.
--
-- 'description', 'importInstanceTaskDetails_description' - A description of the task.
newImportInstanceTaskDetails ::
  ImportInstanceTaskDetails
newImportInstanceTaskDetails =
  ImportInstanceTaskDetails'
    { platform =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      volumes = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The instance operating system.
importInstanceTaskDetails_platform :: Lens.Lens' ImportInstanceTaskDetails (Prelude.Maybe PlatformValues)
importInstanceTaskDetails_platform = Lens.lens (\ImportInstanceTaskDetails' {platform} -> platform) (\s@ImportInstanceTaskDetails' {} a -> s {platform = a} :: ImportInstanceTaskDetails)

-- | The ID of the instance.
importInstanceTaskDetails_instanceId :: Lens.Lens' ImportInstanceTaskDetails (Prelude.Maybe Prelude.Text)
importInstanceTaskDetails_instanceId = Lens.lens (\ImportInstanceTaskDetails' {instanceId} -> instanceId) (\s@ImportInstanceTaskDetails' {} a -> s {instanceId = a} :: ImportInstanceTaskDetails)

-- | The volumes.
importInstanceTaskDetails_volumes :: Lens.Lens' ImportInstanceTaskDetails (Prelude.Maybe [ImportInstanceVolumeDetailItem])
importInstanceTaskDetails_volumes = Lens.lens (\ImportInstanceTaskDetails' {volumes} -> volumes) (\s@ImportInstanceTaskDetails' {} a -> s {volumes = a} :: ImportInstanceTaskDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | A description of the task.
importInstanceTaskDetails_description :: Lens.Lens' ImportInstanceTaskDetails (Prelude.Maybe Prelude.Text)
importInstanceTaskDetails_description = Lens.lens (\ImportInstanceTaskDetails' {description} -> description) (\s@ImportInstanceTaskDetails' {} a -> s {description = a} :: ImportInstanceTaskDetails)

instance Prelude.FromXML ImportInstanceTaskDetails where
  parseXML x =
    ImportInstanceTaskDetails'
      Prelude.<$> (x Prelude..@? "platform")
      Prelude.<*> (x Prelude..@? "instanceId")
      Prelude.<*> ( x Prelude..@? "volumes" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "description")

instance Prelude.Hashable ImportInstanceTaskDetails

instance Prelude.NFData ImportInstanceTaskDetails
