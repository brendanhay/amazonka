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
-- Module      : Amazonka.EC2.Types.ImageRecycleBinInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ImageRecycleBinInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about an AMI that is currently in the Recycle Bin.
--
-- /See:/ 'newImageRecycleBinInfo' smart constructor.
data ImageRecycleBinInfo = ImageRecycleBinInfo'
  { -- | The date and time when the AMI entered the Recycle Bin.
    recycleBinEnterTime :: Prelude.Maybe Core.ISO8601,
    -- | The name of the AMI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the AMI.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the AMI is to be permanently deleted from the
    -- Recycle Bin.
    recycleBinExitTime :: Prelude.Maybe Core.ISO8601,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageRecycleBinInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recycleBinEnterTime', 'imageRecycleBinInfo_recycleBinEnterTime' - The date and time when the AMI entered the Recycle Bin.
--
-- 'name', 'imageRecycleBinInfo_name' - The name of the AMI.
--
-- 'description', 'imageRecycleBinInfo_description' - The description of the AMI.
--
-- 'recycleBinExitTime', 'imageRecycleBinInfo_recycleBinExitTime' - The date and time when the AMI is to be permanently deleted from the
-- Recycle Bin.
--
-- 'imageId', 'imageRecycleBinInfo_imageId' - The ID of the AMI.
newImageRecycleBinInfo ::
  ImageRecycleBinInfo
newImageRecycleBinInfo =
  ImageRecycleBinInfo'
    { recycleBinEnterTime =
        Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      recycleBinExitTime = Prelude.Nothing,
      imageId = Prelude.Nothing
    }

-- | The date and time when the AMI entered the Recycle Bin.
imageRecycleBinInfo_recycleBinEnterTime :: Lens.Lens' ImageRecycleBinInfo (Prelude.Maybe Prelude.UTCTime)
imageRecycleBinInfo_recycleBinEnterTime = Lens.lens (\ImageRecycleBinInfo' {recycleBinEnterTime} -> recycleBinEnterTime) (\s@ImageRecycleBinInfo' {} a -> s {recycleBinEnterTime = a} :: ImageRecycleBinInfo) Prelude.. Lens.mapping Core._Time

-- | The name of the AMI.
imageRecycleBinInfo_name :: Lens.Lens' ImageRecycleBinInfo (Prelude.Maybe Prelude.Text)
imageRecycleBinInfo_name = Lens.lens (\ImageRecycleBinInfo' {name} -> name) (\s@ImageRecycleBinInfo' {} a -> s {name = a} :: ImageRecycleBinInfo)

-- | The description of the AMI.
imageRecycleBinInfo_description :: Lens.Lens' ImageRecycleBinInfo (Prelude.Maybe Prelude.Text)
imageRecycleBinInfo_description = Lens.lens (\ImageRecycleBinInfo' {description} -> description) (\s@ImageRecycleBinInfo' {} a -> s {description = a} :: ImageRecycleBinInfo)

-- | The date and time when the AMI is to be permanently deleted from the
-- Recycle Bin.
imageRecycleBinInfo_recycleBinExitTime :: Lens.Lens' ImageRecycleBinInfo (Prelude.Maybe Prelude.UTCTime)
imageRecycleBinInfo_recycleBinExitTime = Lens.lens (\ImageRecycleBinInfo' {recycleBinExitTime} -> recycleBinExitTime) (\s@ImageRecycleBinInfo' {} a -> s {recycleBinExitTime = a} :: ImageRecycleBinInfo) Prelude.. Lens.mapping Core._Time

-- | The ID of the AMI.
imageRecycleBinInfo_imageId :: Lens.Lens' ImageRecycleBinInfo (Prelude.Maybe Prelude.Text)
imageRecycleBinInfo_imageId = Lens.lens (\ImageRecycleBinInfo' {imageId} -> imageId) (\s@ImageRecycleBinInfo' {} a -> s {imageId = a} :: ImageRecycleBinInfo)

instance Core.FromXML ImageRecycleBinInfo where
  parseXML x =
    ImageRecycleBinInfo'
      Prelude.<$> (x Core..@? "recycleBinEnterTime")
      Prelude.<*> (x Core..@? "name")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "recycleBinExitTime")
      Prelude.<*> (x Core..@? "imageId")

instance Prelude.Hashable ImageRecycleBinInfo where
  hashWithSalt _salt ImageRecycleBinInfo' {..} =
    _salt `Prelude.hashWithSalt` recycleBinEnterTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` recycleBinExitTime
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData ImageRecycleBinInfo where
  rnf ImageRecycleBinInfo' {..} =
    Prelude.rnf recycleBinEnterTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf recycleBinExitTime
      `Prelude.seq` Prelude.rnf imageId
