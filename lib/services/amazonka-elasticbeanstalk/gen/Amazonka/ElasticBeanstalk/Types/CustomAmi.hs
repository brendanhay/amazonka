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
-- Module      : Amazonka.ElasticBeanstalk.Types.CustomAmi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.CustomAmi where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A custom AMI available to platforms.
--
-- /See:/ 'newCustomAmi' smart constructor.
data CustomAmi = CustomAmi'
  { -- | THe ID of the image used to create the custom AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The type of virtualization used to create the custom AMI.
    virtualizationType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomAmi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'customAmi_imageId' - THe ID of the image used to create the custom AMI.
--
-- 'virtualizationType', 'customAmi_virtualizationType' - The type of virtualization used to create the custom AMI.
newCustomAmi ::
  CustomAmi
newCustomAmi =
  CustomAmi'
    { imageId = Prelude.Nothing,
      virtualizationType = Prelude.Nothing
    }

-- | THe ID of the image used to create the custom AMI.
customAmi_imageId :: Lens.Lens' CustomAmi (Prelude.Maybe Prelude.Text)
customAmi_imageId = Lens.lens (\CustomAmi' {imageId} -> imageId) (\s@CustomAmi' {} a -> s {imageId = a} :: CustomAmi)

-- | The type of virtualization used to create the custom AMI.
customAmi_virtualizationType :: Lens.Lens' CustomAmi (Prelude.Maybe Prelude.Text)
customAmi_virtualizationType = Lens.lens (\CustomAmi' {virtualizationType} -> virtualizationType) (\s@CustomAmi' {} a -> s {virtualizationType = a} :: CustomAmi)

instance Data.FromXML CustomAmi where
  parseXML x =
    CustomAmi'
      Prelude.<$> (x Data..@? "ImageId")
      Prelude.<*> (x Data..@? "VirtualizationType")

instance Prelude.Hashable CustomAmi where
  hashWithSalt _salt CustomAmi' {..} =
    _salt
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` virtualizationType

instance Prelude.NFData CustomAmi where
  rnf CustomAmi' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf virtualizationType
