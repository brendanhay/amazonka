{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ValidStorageOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ValidStorageOptions
  ( ValidStorageOptions (..),

    -- * Smart constructor
    mkValidStorageOptions,

    -- * Lenses
    vsoStorageSize,
    vsoProvisionedIOPS,
    vsoIOPSToStorageRatio,
    vsoSupportsStorageAutoscaling,
    vsoStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.DoubleRange
import Network.AWS.RDS.Types.Range

-- | Information about valid modifications that you can make to your DB instance. Contains the result of a successful call to the @DescribeValidDBInstanceModifications@ action.
--
-- /See:/ 'mkValidStorageOptions' smart constructor.
data ValidStorageOptions = ValidStorageOptions'
  { storageSize ::
      Lude.Maybe [Range],
    provisionedIOPS :: Lude.Maybe [Range],
    iopsToStorageRatio :: Lude.Maybe [DoubleRange],
    supportsStorageAutoscaling :: Lude.Maybe Lude.Bool,
    storageType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidStorageOptions' with the minimum fields required to make a request.
--
-- * 'iopsToStorageRatio' - The valid range of Provisioned IOPS to gibibytes of storage multiplier. For example, 3-10, which means that provisioned IOPS can be between 3 and 10 times storage.
-- * 'provisionedIOPS' - The valid range of provisioned IOPS. For example, 1000-20000.
-- * 'storageSize' - The valid range of storage in gibibytes. For example, 100 to 16384.
-- * 'storageType' - The valid storage types for your DB instance. For example, gp2, io1.
-- * 'supportsStorageAutoscaling' - Whether or not Amazon RDS can automatically scale storage for DB instances that use the new instance class.
mkValidStorageOptions ::
  ValidStorageOptions
mkValidStorageOptions =
  ValidStorageOptions'
    { storageSize = Lude.Nothing,
      provisionedIOPS = Lude.Nothing,
      iopsToStorageRatio = Lude.Nothing,
      supportsStorageAutoscaling = Lude.Nothing,
      storageType = Lude.Nothing
    }

-- | The valid range of storage in gibibytes. For example, 100 to 16384.
--
-- /Note:/ Consider using 'storageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoStorageSize :: Lens.Lens' ValidStorageOptions (Lude.Maybe [Range])
vsoStorageSize = Lens.lens (storageSize :: ValidStorageOptions -> Lude.Maybe [Range]) (\s a -> s {storageSize = a} :: ValidStorageOptions)
{-# DEPRECATED vsoStorageSize "Use generic-lens or generic-optics with 'storageSize' instead." #-}

-- | The valid range of provisioned IOPS. For example, 1000-20000.
--
-- /Note:/ Consider using 'provisionedIOPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoProvisionedIOPS :: Lens.Lens' ValidStorageOptions (Lude.Maybe [Range])
vsoProvisionedIOPS = Lens.lens (provisionedIOPS :: ValidStorageOptions -> Lude.Maybe [Range]) (\s a -> s {provisionedIOPS = a} :: ValidStorageOptions)
{-# DEPRECATED vsoProvisionedIOPS "Use generic-lens or generic-optics with 'provisionedIOPS' instead." #-}

-- | The valid range of Provisioned IOPS to gibibytes of storage multiplier. For example, 3-10, which means that provisioned IOPS can be between 3 and 10 times storage.
--
-- /Note:/ Consider using 'iopsToStorageRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoIOPSToStorageRatio :: Lens.Lens' ValidStorageOptions (Lude.Maybe [DoubleRange])
vsoIOPSToStorageRatio = Lens.lens (iopsToStorageRatio :: ValidStorageOptions -> Lude.Maybe [DoubleRange]) (\s a -> s {iopsToStorageRatio = a} :: ValidStorageOptions)
{-# DEPRECATED vsoIOPSToStorageRatio "Use generic-lens or generic-optics with 'iopsToStorageRatio' instead." #-}

-- | Whether or not Amazon RDS can automatically scale storage for DB instances that use the new instance class.
--
-- /Note:/ Consider using 'supportsStorageAutoscaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoSupportsStorageAutoscaling :: Lens.Lens' ValidStorageOptions (Lude.Maybe Lude.Bool)
vsoSupportsStorageAutoscaling = Lens.lens (supportsStorageAutoscaling :: ValidStorageOptions -> Lude.Maybe Lude.Bool) (\s a -> s {supportsStorageAutoscaling = a} :: ValidStorageOptions)
{-# DEPRECATED vsoSupportsStorageAutoscaling "Use generic-lens or generic-optics with 'supportsStorageAutoscaling' instead." #-}

-- | The valid storage types for your DB instance. For example, gp2, io1.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoStorageType :: Lens.Lens' ValidStorageOptions (Lude.Maybe Lude.Text)
vsoStorageType = Lens.lens (storageType :: ValidStorageOptions -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: ValidStorageOptions)
{-# DEPRECATED vsoStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Lude.FromXML ValidStorageOptions where
  parseXML x =
    ValidStorageOptions'
      Lude.<$> ( x Lude..@? "StorageSize" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Range")
               )
      Lude.<*> ( x Lude..@? "ProvisionedIops" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Range")
               )
      Lude.<*> ( x Lude..@? "IopsToStorageRatio" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DoubleRange")
               )
      Lude.<*> (x Lude..@? "SupportsStorageAutoscaling")
      Lude.<*> (x Lude..@? "StorageType")
