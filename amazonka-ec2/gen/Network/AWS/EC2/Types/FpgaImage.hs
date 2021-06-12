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
-- Module      : Network.AWS.EC2.Types.FpgaImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImage where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaImageState
import Network.AWS.EC2.Types.PciId
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes an Amazon FPGA image (AFI).
--
-- /See:/ 'newFpgaImage' smart constructor.
data FpgaImage = FpgaImage'
  { -- | The alias of the AFI owner. Possible values include @self@, @amazon@,
    -- and @aws-marketplace@.
    ownerAlias :: Core.Maybe Core.Text,
    -- | The version of the AWS Shell that was used to create the bitstream.
    shellVersion :: Core.Maybe Core.Text,
    -- | The AWS account ID of the AFI owner.
    ownerId :: Core.Maybe Core.Text,
    -- | The global FPGA image identifier (AGFI ID).
    fpgaImageGlobalId :: Core.Maybe Core.Text,
    -- | Indicates whether data retention support is enabled for the AFI.
    dataRetentionSupport :: Core.Maybe Core.Bool,
    -- | The product codes for the AFI.
    productCodes :: Core.Maybe [ProductCode],
    -- | Information about the state of the AFI.
    state :: Core.Maybe FpgaImageState,
    -- | Information about the PCI bus.
    pciId :: Core.Maybe PciId,
    -- | The name of the AFI.
    name :: Core.Maybe Core.Text,
    -- | The time of the most recent update to the AFI.
    updateTime :: Core.Maybe Core.ISO8601,
    -- | Any tags assigned to the AFI.
    tags :: Core.Maybe [Tag],
    -- | Indicates whether the AFI is public.
    public :: Core.Maybe Core.Bool,
    -- | The date and time the AFI was created.
    createTime :: Core.Maybe Core.ISO8601,
    -- | The description of the AFI.
    description :: Core.Maybe Core.Text,
    -- | The FPGA image identifier (AFI ID).
    fpgaImageId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FpgaImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAlias', 'fpgaImage_ownerAlias' - The alias of the AFI owner. Possible values include @self@, @amazon@,
-- and @aws-marketplace@.
--
-- 'shellVersion', 'fpgaImage_shellVersion' - The version of the AWS Shell that was used to create the bitstream.
--
-- 'ownerId', 'fpgaImage_ownerId' - The AWS account ID of the AFI owner.
--
-- 'fpgaImageGlobalId', 'fpgaImage_fpgaImageGlobalId' - The global FPGA image identifier (AGFI ID).
--
-- 'dataRetentionSupport', 'fpgaImage_dataRetentionSupport' - Indicates whether data retention support is enabled for the AFI.
--
-- 'productCodes', 'fpgaImage_productCodes' - The product codes for the AFI.
--
-- 'state', 'fpgaImage_state' - Information about the state of the AFI.
--
-- 'pciId', 'fpgaImage_pciId' - Information about the PCI bus.
--
-- 'name', 'fpgaImage_name' - The name of the AFI.
--
-- 'updateTime', 'fpgaImage_updateTime' - The time of the most recent update to the AFI.
--
-- 'tags', 'fpgaImage_tags' - Any tags assigned to the AFI.
--
-- 'public', 'fpgaImage_public' - Indicates whether the AFI is public.
--
-- 'createTime', 'fpgaImage_createTime' - The date and time the AFI was created.
--
-- 'description', 'fpgaImage_description' - The description of the AFI.
--
-- 'fpgaImageId', 'fpgaImage_fpgaImageId' - The FPGA image identifier (AFI ID).
newFpgaImage ::
  FpgaImage
newFpgaImage =
  FpgaImage'
    { ownerAlias = Core.Nothing,
      shellVersion = Core.Nothing,
      ownerId = Core.Nothing,
      fpgaImageGlobalId = Core.Nothing,
      dataRetentionSupport = Core.Nothing,
      productCodes = Core.Nothing,
      state = Core.Nothing,
      pciId = Core.Nothing,
      name = Core.Nothing,
      updateTime = Core.Nothing,
      tags = Core.Nothing,
      public = Core.Nothing,
      createTime = Core.Nothing,
      description = Core.Nothing,
      fpgaImageId = Core.Nothing
    }

-- | The alias of the AFI owner. Possible values include @self@, @amazon@,
-- and @aws-marketplace@.
fpgaImage_ownerAlias :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fpgaImage_ownerAlias = Lens.lens (\FpgaImage' {ownerAlias} -> ownerAlias) (\s@FpgaImage' {} a -> s {ownerAlias = a} :: FpgaImage)

-- | The version of the AWS Shell that was used to create the bitstream.
fpgaImage_shellVersion :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fpgaImage_shellVersion = Lens.lens (\FpgaImage' {shellVersion} -> shellVersion) (\s@FpgaImage' {} a -> s {shellVersion = a} :: FpgaImage)

-- | The AWS account ID of the AFI owner.
fpgaImage_ownerId :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fpgaImage_ownerId = Lens.lens (\FpgaImage' {ownerId} -> ownerId) (\s@FpgaImage' {} a -> s {ownerId = a} :: FpgaImage)

-- | The global FPGA image identifier (AGFI ID).
fpgaImage_fpgaImageGlobalId :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fpgaImage_fpgaImageGlobalId = Lens.lens (\FpgaImage' {fpgaImageGlobalId} -> fpgaImageGlobalId) (\s@FpgaImage' {} a -> s {fpgaImageGlobalId = a} :: FpgaImage)

-- | Indicates whether data retention support is enabled for the AFI.
fpgaImage_dataRetentionSupport :: Lens.Lens' FpgaImage (Core.Maybe Core.Bool)
fpgaImage_dataRetentionSupport = Lens.lens (\FpgaImage' {dataRetentionSupport} -> dataRetentionSupport) (\s@FpgaImage' {} a -> s {dataRetentionSupport = a} :: FpgaImage)

-- | The product codes for the AFI.
fpgaImage_productCodes :: Lens.Lens' FpgaImage (Core.Maybe [ProductCode])
fpgaImage_productCodes = Lens.lens (\FpgaImage' {productCodes} -> productCodes) (\s@FpgaImage' {} a -> s {productCodes = a} :: FpgaImage) Core.. Lens.mapping Lens._Coerce

-- | Information about the state of the AFI.
fpgaImage_state :: Lens.Lens' FpgaImage (Core.Maybe FpgaImageState)
fpgaImage_state = Lens.lens (\FpgaImage' {state} -> state) (\s@FpgaImage' {} a -> s {state = a} :: FpgaImage)

-- | Information about the PCI bus.
fpgaImage_pciId :: Lens.Lens' FpgaImage (Core.Maybe PciId)
fpgaImage_pciId = Lens.lens (\FpgaImage' {pciId} -> pciId) (\s@FpgaImage' {} a -> s {pciId = a} :: FpgaImage)

-- | The name of the AFI.
fpgaImage_name :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fpgaImage_name = Lens.lens (\FpgaImage' {name} -> name) (\s@FpgaImage' {} a -> s {name = a} :: FpgaImage)

-- | The time of the most recent update to the AFI.
fpgaImage_updateTime :: Lens.Lens' FpgaImage (Core.Maybe Core.UTCTime)
fpgaImage_updateTime = Lens.lens (\FpgaImage' {updateTime} -> updateTime) (\s@FpgaImage' {} a -> s {updateTime = a} :: FpgaImage) Core.. Lens.mapping Core._Time

-- | Any tags assigned to the AFI.
fpgaImage_tags :: Lens.Lens' FpgaImage (Core.Maybe [Tag])
fpgaImage_tags = Lens.lens (\FpgaImage' {tags} -> tags) (\s@FpgaImage' {} a -> s {tags = a} :: FpgaImage) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the AFI is public.
fpgaImage_public :: Lens.Lens' FpgaImage (Core.Maybe Core.Bool)
fpgaImage_public = Lens.lens (\FpgaImage' {public} -> public) (\s@FpgaImage' {} a -> s {public = a} :: FpgaImage)

-- | The date and time the AFI was created.
fpgaImage_createTime :: Lens.Lens' FpgaImage (Core.Maybe Core.UTCTime)
fpgaImage_createTime = Lens.lens (\FpgaImage' {createTime} -> createTime) (\s@FpgaImage' {} a -> s {createTime = a} :: FpgaImage) Core.. Lens.mapping Core._Time

-- | The description of the AFI.
fpgaImage_description :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fpgaImage_description = Lens.lens (\FpgaImage' {description} -> description) (\s@FpgaImage' {} a -> s {description = a} :: FpgaImage)

-- | The FPGA image identifier (AFI ID).
fpgaImage_fpgaImageId :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fpgaImage_fpgaImageId = Lens.lens (\FpgaImage' {fpgaImageId} -> fpgaImageId) (\s@FpgaImage' {} a -> s {fpgaImageId = a} :: FpgaImage)

instance Core.FromXML FpgaImage where
  parseXML x =
    FpgaImage'
      Core.<$> (x Core..@? "ownerAlias")
      Core.<*> (x Core..@? "shellVersion")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "fpgaImageGlobalId")
      Core.<*> (x Core..@? "dataRetentionSupport")
      Core.<*> ( x Core..@? "productCodes" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "pciId")
      Core.<*> (x Core..@? "name")
      Core.<*> (x Core..@? "updateTime")
      Core.<*> ( x Core..@? "tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "public")
      Core.<*> (x Core..@? "createTime")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "fpgaImageId")

instance Core.Hashable FpgaImage

instance Core.NFData FpgaImage
