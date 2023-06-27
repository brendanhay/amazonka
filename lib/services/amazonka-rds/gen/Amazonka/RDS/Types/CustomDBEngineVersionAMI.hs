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
-- Module      : Amazonka.RDS.Types.CustomDBEngineVersionAMI
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.CustomDBEngineVersionAMI where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A value that indicates the AMI information.
--
-- /See:/ 'newCustomDBEngineVersionAMI' smart constructor.
data CustomDBEngineVersionAMI = CustomDBEngineVersionAMI'
  { -- | A value that indicates the ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates the status of a custom engine version (CEV).
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDBEngineVersionAMI' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'customDBEngineVersionAMI_imageId' - A value that indicates the ID of the AMI.
--
-- 'status', 'customDBEngineVersionAMI_status' - A value that indicates the status of a custom engine version (CEV).
newCustomDBEngineVersionAMI ::
  CustomDBEngineVersionAMI
newCustomDBEngineVersionAMI =
  CustomDBEngineVersionAMI'
    { imageId =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | A value that indicates the ID of the AMI.
customDBEngineVersionAMI_imageId :: Lens.Lens' CustomDBEngineVersionAMI (Prelude.Maybe Prelude.Text)
customDBEngineVersionAMI_imageId = Lens.lens (\CustomDBEngineVersionAMI' {imageId} -> imageId) (\s@CustomDBEngineVersionAMI' {} a -> s {imageId = a} :: CustomDBEngineVersionAMI)

-- | A value that indicates the status of a custom engine version (CEV).
customDBEngineVersionAMI_status :: Lens.Lens' CustomDBEngineVersionAMI (Prelude.Maybe Prelude.Text)
customDBEngineVersionAMI_status = Lens.lens (\CustomDBEngineVersionAMI' {status} -> status) (\s@CustomDBEngineVersionAMI' {} a -> s {status = a} :: CustomDBEngineVersionAMI)

instance Data.FromXML CustomDBEngineVersionAMI where
  parseXML x =
    CustomDBEngineVersionAMI'
      Prelude.<$> (x Data..@? "ImageId")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable CustomDBEngineVersionAMI where
  hashWithSalt _salt CustomDBEngineVersionAMI' {..} =
    _salt
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` status

instance Prelude.NFData CustomDBEngineVersionAMI where
  rnf CustomDBEngineVersionAMI' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf status
