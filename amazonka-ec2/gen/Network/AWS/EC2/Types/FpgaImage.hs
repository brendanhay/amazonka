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
-- Module      : Network.AWS.EC2.Types.FpgaImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImage where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaImageState
import Network.AWS.EC2.Types.PciId
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon FPGA image (AFI).
--
-- /See:/ 'newFpgaImage' smart constructor.
data FpgaImage = FpgaImage'
  { -- | The alias of the AFI owner. Possible values include @self@, @amazon@,
    -- and @aws-marketplace@.
    ownerAlias :: Prelude.Maybe Prelude.Text,
    -- | The version of the AWS Shell that was used to create the bitstream.
    shellVersion :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID of the AFI owner.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The global FPGA image identifier (AGFI ID).
    fpgaImageGlobalId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether data retention support is enabled for the AFI.
    dataRetentionSupport :: Prelude.Maybe Prelude.Bool,
    -- | The product codes for the AFI.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | Information about the state of the AFI.
    state :: Prelude.Maybe FpgaImageState,
    -- | Information about the PCI bus.
    pciId :: Prelude.Maybe PciId,
    -- | The name of the AFI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time of the most recent update to the AFI.
    updateTime :: Prelude.Maybe Prelude.ISO8601,
    -- | Any tags assigned to the AFI.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates whether the AFI is public.
    public :: Prelude.Maybe Prelude.Bool,
    -- | The date and time the AFI was created.
    createTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The description of the AFI.
    description :: Prelude.Maybe Prelude.Text,
    -- | The FPGA image identifier (AFI ID).
    fpgaImageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ownerAlias = Prelude.Nothing,
      shellVersion = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      fpgaImageGlobalId = Prelude.Nothing,
      dataRetentionSupport = Prelude.Nothing,
      productCodes = Prelude.Nothing,
      state = Prelude.Nothing,
      pciId = Prelude.Nothing,
      name = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      public = Prelude.Nothing,
      createTime = Prelude.Nothing,
      description = Prelude.Nothing,
      fpgaImageId = Prelude.Nothing
    }

-- | The alias of the AFI owner. Possible values include @self@, @amazon@,
-- and @aws-marketplace@.
fpgaImage_ownerAlias :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_ownerAlias = Lens.lens (\FpgaImage' {ownerAlias} -> ownerAlias) (\s@FpgaImage' {} a -> s {ownerAlias = a} :: FpgaImage)

-- | The version of the AWS Shell that was used to create the bitstream.
fpgaImage_shellVersion :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_shellVersion = Lens.lens (\FpgaImage' {shellVersion} -> shellVersion) (\s@FpgaImage' {} a -> s {shellVersion = a} :: FpgaImage)

-- | The AWS account ID of the AFI owner.
fpgaImage_ownerId :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_ownerId = Lens.lens (\FpgaImage' {ownerId} -> ownerId) (\s@FpgaImage' {} a -> s {ownerId = a} :: FpgaImage)

-- | The global FPGA image identifier (AGFI ID).
fpgaImage_fpgaImageGlobalId :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_fpgaImageGlobalId = Lens.lens (\FpgaImage' {fpgaImageGlobalId} -> fpgaImageGlobalId) (\s@FpgaImage' {} a -> s {fpgaImageGlobalId = a} :: FpgaImage)

-- | Indicates whether data retention support is enabled for the AFI.
fpgaImage_dataRetentionSupport :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Bool)
fpgaImage_dataRetentionSupport = Lens.lens (\FpgaImage' {dataRetentionSupport} -> dataRetentionSupport) (\s@FpgaImage' {} a -> s {dataRetentionSupport = a} :: FpgaImage)

-- | The product codes for the AFI.
fpgaImage_productCodes :: Lens.Lens' FpgaImage (Prelude.Maybe [ProductCode])
fpgaImage_productCodes = Lens.lens (\FpgaImage' {productCodes} -> productCodes) (\s@FpgaImage' {} a -> s {productCodes = a} :: FpgaImage) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the state of the AFI.
fpgaImage_state :: Lens.Lens' FpgaImage (Prelude.Maybe FpgaImageState)
fpgaImage_state = Lens.lens (\FpgaImage' {state} -> state) (\s@FpgaImage' {} a -> s {state = a} :: FpgaImage)

-- | Information about the PCI bus.
fpgaImage_pciId :: Lens.Lens' FpgaImage (Prelude.Maybe PciId)
fpgaImage_pciId = Lens.lens (\FpgaImage' {pciId} -> pciId) (\s@FpgaImage' {} a -> s {pciId = a} :: FpgaImage)

-- | The name of the AFI.
fpgaImage_name :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_name = Lens.lens (\FpgaImage' {name} -> name) (\s@FpgaImage' {} a -> s {name = a} :: FpgaImage)

-- | The time of the most recent update to the AFI.
fpgaImage_updateTime :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.UTCTime)
fpgaImage_updateTime = Lens.lens (\FpgaImage' {updateTime} -> updateTime) (\s@FpgaImage' {} a -> s {updateTime = a} :: FpgaImage) Prelude.. Lens.mapping Prelude._Time

-- | Any tags assigned to the AFI.
fpgaImage_tags :: Lens.Lens' FpgaImage (Prelude.Maybe [Tag])
fpgaImage_tags = Lens.lens (\FpgaImage' {tags} -> tags) (\s@FpgaImage' {} a -> s {tags = a} :: FpgaImage) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether the AFI is public.
fpgaImage_public :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Bool)
fpgaImage_public = Lens.lens (\FpgaImage' {public} -> public) (\s@FpgaImage' {} a -> s {public = a} :: FpgaImage)

-- | The date and time the AFI was created.
fpgaImage_createTime :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.UTCTime)
fpgaImage_createTime = Lens.lens (\FpgaImage' {createTime} -> createTime) (\s@FpgaImage' {} a -> s {createTime = a} :: FpgaImage) Prelude.. Lens.mapping Prelude._Time

-- | The description of the AFI.
fpgaImage_description :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_description = Lens.lens (\FpgaImage' {description} -> description) (\s@FpgaImage' {} a -> s {description = a} :: FpgaImage)

-- | The FPGA image identifier (AFI ID).
fpgaImage_fpgaImageId :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_fpgaImageId = Lens.lens (\FpgaImage' {fpgaImageId} -> fpgaImageId) (\s@FpgaImage' {} a -> s {fpgaImageId = a} :: FpgaImage)

instance Prelude.FromXML FpgaImage where
  parseXML x =
    FpgaImage'
      Prelude.<$> (x Prelude..@? "ownerAlias")
      Prelude.<*> (x Prelude..@? "shellVersion")
      Prelude.<*> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "fpgaImageGlobalId")
      Prelude.<*> (x Prelude..@? "dataRetentionSupport")
      Prelude.<*> ( x Prelude..@? "productCodes"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "pciId")
      Prelude.<*> (x Prelude..@? "name")
      Prelude.<*> (x Prelude..@? "updateTime")
      Prelude.<*> ( x Prelude..@? "tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "public")
      Prelude.<*> (x Prelude..@? "createTime")
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "fpgaImageId")

instance Prelude.Hashable FpgaImage

instance Prelude.NFData FpgaImage
