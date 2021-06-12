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
-- Module      : Network.AWS.ElasticBeanstalk.Types.CustomAmi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.CustomAmi where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A custom AMI available to platforms.
--
-- /See:/ 'newCustomAmi' smart constructor.
data CustomAmi = CustomAmi'
  { -- | The type of virtualization used to create the custom AMI.
    virtualizationType :: Core.Maybe Core.Text,
    -- | THe ID of the image used to create the custom AMI.
    imageId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CustomAmi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualizationType', 'customAmi_virtualizationType' - The type of virtualization used to create the custom AMI.
--
-- 'imageId', 'customAmi_imageId' - THe ID of the image used to create the custom AMI.
newCustomAmi ::
  CustomAmi
newCustomAmi =
  CustomAmi'
    { virtualizationType = Core.Nothing,
      imageId = Core.Nothing
    }

-- | The type of virtualization used to create the custom AMI.
customAmi_virtualizationType :: Lens.Lens' CustomAmi (Core.Maybe Core.Text)
customAmi_virtualizationType = Lens.lens (\CustomAmi' {virtualizationType} -> virtualizationType) (\s@CustomAmi' {} a -> s {virtualizationType = a} :: CustomAmi)

-- | THe ID of the image used to create the custom AMI.
customAmi_imageId :: Lens.Lens' CustomAmi (Core.Maybe Core.Text)
customAmi_imageId = Lens.lens (\CustomAmi' {imageId} -> imageId) (\s@CustomAmi' {} a -> s {imageId = a} :: CustomAmi)

instance Core.FromXML CustomAmi where
  parseXML x =
    CustomAmi'
      Core.<$> (x Core..@? "VirtualizationType")
      Core.<*> (x Core..@? "ImageId")

instance Core.Hashable CustomAmi

instance Core.NFData CustomAmi
