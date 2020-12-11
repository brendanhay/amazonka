-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryConfiguration
  ( InventoryConfiguration (..),

    -- * Smart constructor
    mkInventoryConfiguration,

    -- * Lenses
    icOptionalFields,
    icFilter,
    icDestination,
    icIsEnabled,
    icId,
    icIncludedObjectVersions,
    icSchedule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryDestination
import Network.AWS.S3.Types.InventoryFilter
import Network.AWS.S3.Types.InventoryIncludedObjectVersions
import Network.AWS.S3.Types.InventoryOptionalField
import Network.AWS.S3.Types.InventorySchedule

-- | Specifies the inventory configuration for an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETInventoryConfig.html GET Bucket inventory> in the /Amazon Simple Storage Service API Reference/ .
--
-- /See:/ 'mkInventoryConfiguration' smart constructor.
data InventoryConfiguration = InventoryConfiguration'
  { optionalFields ::
      Lude.Maybe [InventoryOptionalField],
    filter :: Lude.Maybe InventoryFilter,
    destination :: InventoryDestination,
    isEnabled :: Lude.Bool,
    id :: Lude.Text,
    includedObjectVersions ::
      InventoryIncludedObjectVersions,
    schedule :: InventorySchedule
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryConfiguration' with the minimum fields required to make a request.
--
-- * 'destination' - Contains information about where to publish the inventory results.
-- * 'filter' - Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
-- * 'id' - The ID used to identify the inventory configuration.
-- * 'includedObjectVersions' - Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
-- * 'isEnabled' - Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
-- * 'optionalFields' - Contains the optional fields that are included in the inventory results.
-- * 'schedule' - Specifies the schedule for generating inventory results.
mkInventoryConfiguration ::
  -- | 'destination'
  InventoryDestination ->
  -- | 'isEnabled'
  Lude.Bool ->
  -- | 'id'
  Lude.Text ->
  -- | 'includedObjectVersions'
  InventoryIncludedObjectVersions ->
  -- | 'schedule'
  InventorySchedule ->
  InventoryConfiguration
mkInventoryConfiguration
  pDestination_
  pIsEnabled_
  pId_
  pIncludedObjectVersions_
  pSchedule_ =
    InventoryConfiguration'
      { optionalFields = Lude.Nothing,
        filter = Lude.Nothing,
        destination = pDestination_,
        isEnabled = pIsEnabled_,
        id = pId_,
        includedObjectVersions = pIncludedObjectVersions_,
        schedule = pSchedule_
      }

-- | Contains the optional fields that are included in the inventory results.
--
-- /Note:/ Consider using 'optionalFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icOptionalFields :: Lens.Lens' InventoryConfiguration (Lude.Maybe [InventoryOptionalField])
icOptionalFields = Lens.lens (optionalFields :: InventoryConfiguration -> Lude.Maybe [InventoryOptionalField]) (\s a -> s {optionalFields = a} :: InventoryConfiguration)
{-# DEPRECATED icOptionalFields "Use generic-lens or generic-optics with 'optionalFields' instead." #-}

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icFilter :: Lens.Lens' InventoryConfiguration (Lude.Maybe InventoryFilter)
icFilter = Lens.lens (filter :: InventoryConfiguration -> Lude.Maybe InventoryFilter) (\s a -> s {filter = a} :: InventoryConfiguration)
{-# DEPRECATED icFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Contains information about where to publish the inventory results.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icDestination :: Lens.Lens' InventoryConfiguration InventoryDestination
icDestination = Lens.lens (destination :: InventoryConfiguration -> InventoryDestination) (\s a -> s {destination = a} :: InventoryConfiguration)
{-# DEPRECATED icDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
--
-- /Note:/ Consider using 'isEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIsEnabled :: Lens.Lens' InventoryConfiguration Lude.Bool
icIsEnabled = Lens.lens (isEnabled :: InventoryConfiguration -> Lude.Bool) (\s a -> s {isEnabled = a} :: InventoryConfiguration)
{-# DEPRECATED icIsEnabled "Use generic-lens or generic-optics with 'isEnabled' instead." #-}

-- | The ID used to identify the inventory configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icId :: Lens.Lens' InventoryConfiguration Lude.Text
icId = Lens.lens (id :: InventoryConfiguration -> Lude.Text) (\s a -> s {id = a} :: InventoryConfiguration)
{-# DEPRECATED icId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
--
-- /Note:/ Consider using 'includedObjectVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIncludedObjectVersions :: Lens.Lens' InventoryConfiguration InventoryIncludedObjectVersions
icIncludedObjectVersions = Lens.lens (includedObjectVersions :: InventoryConfiguration -> InventoryIncludedObjectVersions) (\s a -> s {includedObjectVersions = a} :: InventoryConfiguration)
{-# DEPRECATED icIncludedObjectVersions "Use generic-lens or generic-optics with 'includedObjectVersions' instead." #-}

-- | Specifies the schedule for generating inventory results.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSchedule :: Lens.Lens' InventoryConfiguration InventorySchedule
icSchedule = Lens.lens (schedule :: InventoryConfiguration -> InventorySchedule) (\s a -> s {schedule = a} :: InventoryConfiguration)
{-# DEPRECATED icSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

instance Lude.FromXML InventoryConfiguration where
  parseXML x =
    InventoryConfiguration'
      Lude.<$> ( x Lude..@? "OptionalFields" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Field")
               )
      Lude.<*> (x Lude..@? "Filter")
      Lude.<*> (x Lude..@ "Destination")
      Lude.<*> (x Lude..@ "IsEnabled")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "IncludedObjectVersions")
      Lude.<*> (x Lude..@ "Schedule")

instance Lude.ToXML InventoryConfiguration where
  toXML InventoryConfiguration' {..} =
    Lude.mconcat
      [ "OptionalFields"
          Lude.@= Lude.toXML (Lude.toXMLList "Field" Lude.<$> optionalFields),
        "Filter" Lude.@= filter,
        "Destination" Lude.@= destination,
        "IsEnabled" Lude.@= isEnabled,
        "Id" Lude.@= id,
        "IncludedObjectVersions" Lude.@= includedObjectVersions,
        "Schedule" Lude.@= schedule
      ]
