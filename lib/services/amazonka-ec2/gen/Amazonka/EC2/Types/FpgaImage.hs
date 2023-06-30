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
-- Module      : Amazonka.EC2.Types.FpgaImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FpgaImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FpgaImageState
import Amazonka.EC2.Types.PciId
import Amazonka.EC2.Types.ProductCode
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon FPGA image (AFI).
--
-- /See:/ 'newFpgaImage' smart constructor.
data FpgaImage = FpgaImage'
  { -- | The date and time the AFI was created.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | Indicates whether data retention support is enabled for the AFI.
    dataRetentionSupport :: Prelude.Maybe Prelude.Bool,
    -- | The description of the AFI.
    description :: Prelude.Maybe Prelude.Text,
    -- | The global FPGA image identifier (AGFI ID).
    fpgaImageGlobalId :: Prelude.Maybe Prelude.Text,
    -- | The FPGA image identifier (AFI ID).
    fpgaImageId :: Prelude.Maybe Prelude.Text,
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The name of the AFI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The alias of the AFI owner. Possible values include @self@, @amazon@,
    -- and @aws-marketplace@.
    ownerAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the AFI.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Information about the PCI bus.
    pciId :: Prelude.Maybe PciId,
    -- | The product codes for the AFI.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | Indicates whether the AFI is public.
    public :: Prelude.Maybe Prelude.Bool,
    -- | The version of the Amazon Web Services Shell that was used to create the
    -- bitstream.
    shellVersion :: Prelude.Maybe Prelude.Text,
    -- | Information about the state of the AFI.
    state :: Prelude.Maybe FpgaImageState,
    -- | Any tags assigned to the AFI.
    tags :: Prelude.Maybe [Tag],
    -- | The time of the most recent update to the AFI.
    updateTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FpgaImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'fpgaImage_createTime' - The date and time the AFI was created.
--
-- 'dataRetentionSupport', 'fpgaImage_dataRetentionSupport' - Indicates whether data retention support is enabled for the AFI.
--
-- 'description', 'fpgaImage_description' - The description of the AFI.
--
-- 'fpgaImageGlobalId', 'fpgaImage_fpgaImageGlobalId' - The global FPGA image identifier (AGFI ID).
--
-- 'fpgaImageId', 'fpgaImage_fpgaImageId' - The FPGA image identifier (AFI ID).
--
-- 'instanceTypes', 'fpgaImage_instanceTypes' - Undocumented member.
--
-- 'name', 'fpgaImage_name' - The name of the AFI.
--
-- 'ownerAlias', 'fpgaImage_ownerAlias' - The alias of the AFI owner. Possible values include @self@, @amazon@,
-- and @aws-marketplace@.
--
-- 'ownerId', 'fpgaImage_ownerId' - The ID of the Amazon Web Services account that owns the AFI.
--
-- 'pciId', 'fpgaImage_pciId' - Information about the PCI bus.
--
-- 'productCodes', 'fpgaImage_productCodes' - The product codes for the AFI.
--
-- 'public', 'fpgaImage_public' - Indicates whether the AFI is public.
--
-- 'shellVersion', 'fpgaImage_shellVersion' - The version of the Amazon Web Services Shell that was used to create the
-- bitstream.
--
-- 'state', 'fpgaImage_state' - Information about the state of the AFI.
--
-- 'tags', 'fpgaImage_tags' - Any tags assigned to the AFI.
--
-- 'updateTime', 'fpgaImage_updateTime' - The time of the most recent update to the AFI.
newFpgaImage ::
  FpgaImage
newFpgaImage =
  FpgaImage'
    { createTime = Prelude.Nothing,
      dataRetentionSupport = Prelude.Nothing,
      description = Prelude.Nothing,
      fpgaImageGlobalId = Prelude.Nothing,
      fpgaImageId = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      name = Prelude.Nothing,
      ownerAlias = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      pciId = Prelude.Nothing,
      productCodes = Prelude.Nothing,
      public = Prelude.Nothing,
      shellVersion = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      updateTime = Prelude.Nothing
    }

-- | The date and time the AFI was created.
fpgaImage_createTime :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.UTCTime)
fpgaImage_createTime = Lens.lens (\FpgaImage' {createTime} -> createTime) (\s@FpgaImage' {} a -> s {createTime = a} :: FpgaImage) Prelude.. Lens.mapping Data._Time

-- | Indicates whether data retention support is enabled for the AFI.
fpgaImage_dataRetentionSupport :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Bool)
fpgaImage_dataRetentionSupport = Lens.lens (\FpgaImage' {dataRetentionSupport} -> dataRetentionSupport) (\s@FpgaImage' {} a -> s {dataRetentionSupport = a} :: FpgaImage)

-- | The description of the AFI.
fpgaImage_description :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_description = Lens.lens (\FpgaImage' {description} -> description) (\s@FpgaImage' {} a -> s {description = a} :: FpgaImage)

-- | The global FPGA image identifier (AGFI ID).
fpgaImage_fpgaImageGlobalId :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_fpgaImageGlobalId = Lens.lens (\FpgaImage' {fpgaImageGlobalId} -> fpgaImageGlobalId) (\s@FpgaImage' {} a -> s {fpgaImageGlobalId = a} :: FpgaImage)

-- | The FPGA image identifier (AFI ID).
fpgaImage_fpgaImageId :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_fpgaImageId = Lens.lens (\FpgaImage' {fpgaImageId} -> fpgaImageId) (\s@FpgaImage' {} a -> s {fpgaImageId = a} :: FpgaImage)

-- | Undocumented member.
fpgaImage_instanceTypes :: Lens.Lens' FpgaImage (Prelude.Maybe [Prelude.Text])
fpgaImage_instanceTypes = Lens.lens (\FpgaImage' {instanceTypes} -> instanceTypes) (\s@FpgaImage' {} a -> s {instanceTypes = a} :: FpgaImage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the AFI.
fpgaImage_name :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_name = Lens.lens (\FpgaImage' {name} -> name) (\s@FpgaImage' {} a -> s {name = a} :: FpgaImage)

-- | The alias of the AFI owner. Possible values include @self@, @amazon@,
-- and @aws-marketplace@.
fpgaImage_ownerAlias :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_ownerAlias = Lens.lens (\FpgaImage' {ownerAlias} -> ownerAlias) (\s@FpgaImage' {} a -> s {ownerAlias = a} :: FpgaImage)

-- | The ID of the Amazon Web Services account that owns the AFI.
fpgaImage_ownerId :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_ownerId = Lens.lens (\FpgaImage' {ownerId} -> ownerId) (\s@FpgaImage' {} a -> s {ownerId = a} :: FpgaImage)

-- | Information about the PCI bus.
fpgaImage_pciId :: Lens.Lens' FpgaImage (Prelude.Maybe PciId)
fpgaImage_pciId = Lens.lens (\FpgaImage' {pciId} -> pciId) (\s@FpgaImage' {} a -> s {pciId = a} :: FpgaImage)

-- | The product codes for the AFI.
fpgaImage_productCodes :: Lens.Lens' FpgaImage (Prelude.Maybe [ProductCode])
fpgaImage_productCodes = Lens.lens (\FpgaImage' {productCodes} -> productCodes) (\s@FpgaImage' {} a -> s {productCodes = a} :: FpgaImage) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the AFI is public.
fpgaImage_public :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Bool)
fpgaImage_public = Lens.lens (\FpgaImage' {public} -> public) (\s@FpgaImage' {} a -> s {public = a} :: FpgaImage)

-- | The version of the Amazon Web Services Shell that was used to create the
-- bitstream.
fpgaImage_shellVersion :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.Text)
fpgaImage_shellVersion = Lens.lens (\FpgaImage' {shellVersion} -> shellVersion) (\s@FpgaImage' {} a -> s {shellVersion = a} :: FpgaImage)

-- | Information about the state of the AFI.
fpgaImage_state :: Lens.Lens' FpgaImage (Prelude.Maybe FpgaImageState)
fpgaImage_state = Lens.lens (\FpgaImage' {state} -> state) (\s@FpgaImage' {} a -> s {state = a} :: FpgaImage)

-- | Any tags assigned to the AFI.
fpgaImage_tags :: Lens.Lens' FpgaImage (Prelude.Maybe [Tag])
fpgaImage_tags = Lens.lens (\FpgaImage' {tags} -> tags) (\s@FpgaImage' {} a -> s {tags = a} :: FpgaImage) Prelude.. Lens.mapping Lens.coerced

-- | The time of the most recent update to the AFI.
fpgaImage_updateTime :: Lens.Lens' FpgaImage (Prelude.Maybe Prelude.UTCTime)
fpgaImage_updateTime = Lens.lens (\FpgaImage' {updateTime} -> updateTime) (\s@FpgaImage' {} a -> s {updateTime = a} :: FpgaImage) Prelude.. Lens.mapping Data._Time

instance Data.FromXML FpgaImage where
  parseXML x =
    FpgaImage'
      Prelude.<$> (x Data..@? "createTime")
      Prelude.<*> (x Data..@? "dataRetentionSupport")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "fpgaImageGlobalId")
      Prelude.<*> (x Data..@? "fpgaImageId")
      Prelude.<*> ( x
                      Data..@? "instanceTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "name")
      Prelude.<*> (x Data..@? "ownerAlias")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "pciId")
      Prelude.<*> ( x
                      Data..@? "productCodes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "public")
      Prelude.<*> (x Data..@? "shellVersion")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x
                      Data..@? "tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "updateTime")

instance Prelude.Hashable FpgaImage where
  hashWithSalt _salt FpgaImage' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` dataRetentionSupport
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fpgaImageGlobalId
      `Prelude.hashWithSalt` fpgaImageId
      `Prelude.hashWithSalt` instanceTypes
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ownerAlias
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` pciId
      `Prelude.hashWithSalt` productCodes
      `Prelude.hashWithSalt` public
      `Prelude.hashWithSalt` shellVersion
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData FpgaImage where
  rnf FpgaImage' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf dataRetentionSupport
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf fpgaImageGlobalId
      `Prelude.seq` Prelude.rnf fpgaImageId
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerAlias
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf pciId
      `Prelude.seq` Prelude.rnf productCodes
      `Prelude.seq` Prelude.rnf public
      `Prelude.seq` Prelude.rnf shellVersion
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updateTime
