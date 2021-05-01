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
-- Module      : Network.AWS.RDS.Types.ValidStorageOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ValidStorageOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.DoubleRange
import Network.AWS.RDS.Types.Range

-- | Information about valid modifications that you can make to your DB
-- instance. Contains the result of a successful call to the
-- @DescribeValidDBInstanceModifications@ action.
--
-- /See:/ 'newValidStorageOptions' smart constructor.
data ValidStorageOptions = ValidStorageOptions'
  { -- | The valid storage types for your DB instance. For example, gp2, io1.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The valid range of provisioned IOPS. For example, 1000-20000.
    provisionedIops :: Prelude.Maybe [Range],
    -- | Whether or not Amazon RDS can automatically scale storage for DB
    -- instances that use the new instance class.
    supportsStorageAutoscaling :: Prelude.Maybe Prelude.Bool,
    -- | The valid range of storage in gibibytes. For example, 100 to 16384.
    storageSize :: Prelude.Maybe [Range],
    -- | The valid range of Provisioned IOPS to gibibytes of storage multiplier.
    -- For example, 3-10, which means that provisioned IOPS can be between 3
    -- and 10 times storage.
    iopsToStorageRatio :: Prelude.Maybe [DoubleRange]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ValidStorageOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageType', 'validStorageOptions_storageType' - The valid storage types for your DB instance. For example, gp2, io1.
--
-- 'provisionedIops', 'validStorageOptions_provisionedIops' - The valid range of provisioned IOPS. For example, 1000-20000.
--
-- 'supportsStorageAutoscaling', 'validStorageOptions_supportsStorageAutoscaling' - Whether or not Amazon RDS can automatically scale storage for DB
-- instances that use the new instance class.
--
-- 'storageSize', 'validStorageOptions_storageSize' - The valid range of storage in gibibytes. For example, 100 to 16384.
--
-- 'iopsToStorageRatio', 'validStorageOptions_iopsToStorageRatio' - The valid range of Provisioned IOPS to gibibytes of storage multiplier.
-- For example, 3-10, which means that provisioned IOPS can be between 3
-- and 10 times storage.
newValidStorageOptions ::
  ValidStorageOptions
newValidStorageOptions =
  ValidStorageOptions'
    { storageType = Prelude.Nothing,
      provisionedIops = Prelude.Nothing,
      supportsStorageAutoscaling = Prelude.Nothing,
      storageSize = Prelude.Nothing,
      iopsToStorageRatio = Prelude.Nothing
    }

-- | The valid storage types for your DB instance. For example, gp2, io1.
validStorageOptions_storageType :: Lens.Lens' ValidStorageOptions (Prelude.Maybe Prelude.Text)
validStorageOptions_storageType = Lens.lens (\ValidStorageOptions' {storageType} -> storageType) (\s@ValidStorageOptions' {} a -> s {storageType = a} :: ValidStorageOptions)

-- | The valid range of provisioned IOPS. For example, 1000-20000.
validStorageOptions_provisionedIops :: Lens.Lens' ValidStorageOptions (Prelude.Maybe [Range])
validStorageOptions_provisionedIops = Lens.lens (\ValidStorageOptions' {provisionedIops} -> provisionedIops) (\s@ValidStorageOptions' {} a -> s {provisionedIops = a} :: ValidStorageOptions) Prelude.. Lens.mapping Prelude._Coerce

-- | Whether or not Amazon RDS can automatically scale storage for DB
-- instances that use the new instance class.
validStorageOptions_supportsStorageAutoscaling :: Lens.Lens' ValidStorageOptions (Prelude.Maybe Prelude.Bool)
validStorageOptions_supportsStorageAutoscaling = Lens.lens (\ValidStorageOptions' {supportsStorageAutoscaling} -> supportsStorageAutoscaling) (\s@ValidStorageOptions' {} a -> s {supportsStorageAutoscaling = a} :: ValidStorageOptions)

-- | The valid range of storage in gibibytes. For example, 100 to 16384.
validStorageOptions_storageSize :: Lens.Lens' ValidStorageOptions (Prelude.Maybe [Range])
validStorageOptions_storageSize = Lens.lens (\ValidStorageOptions' {storageSize} -> storageSize) (\s@ValidStorageOptions' {} a -> s {storageSize = a} :: ValidStorageOptions) Prelude.. Lens.mapping Prelude._Coerce

-- | The valid range of Provisioned IOPS to gibibytes of storage multiplier.
-- For example, 3-10, which means that provisioned IOPS can be between 3
-- and 10 times storage.
validStorageOptions_iopsToStorageRatio :: Lens.Lens' ValidStorageOptions (Prelude.Maybe [DoubleRange])
validStorageOptions_iopsToStorageRatio = Lens.lens (\ValidStorageOptions' {iopsToStorageRatio} -> iopsToStorageRatio) (\s@ValidStorageOptions' {} a -> s {iopsToStorageRatio = a} :: ValidStorageOptions) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML ValidStorageOptions where
  parseXML x =
    ValidStorageOptions'
      Prelude.<$> (x Prelude..@? "StorageType")
      Prelude.<*> ( x Prelude..@? "ProvisionedIops"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Range")
                  )
      Prelude.<*> (x Prelude..@? "SupportsStorageAutoscaling")
      Prelude.<*> ( x Prelude..@? "StorageSize"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Range")
                  )
      Prelude.<*> ( x Prelude..@? "IopsToStorageRatio"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "DoubleRange")
                  )

instance Prelude.Hashable ValidStorageOptions

instance Prelude.NFData ValidStorageOptions
