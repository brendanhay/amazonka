{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    icIncludedObjectVersions,
    icDestination,
    icSchedule,
    icIsEnabled,
    icOptionalFields,
    icId,
    icFilter,
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
  { -- | Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
    includedObjectVersions :: InventoryIncludedObjectVersions,
    -- | Contains information about where to publish the inventory results.
    destination :: InventoryDestination,
    -- | Specifies the schedule for generating inventory results.
    schedule :: InventorySchedule,
    -- | Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
    isEnabled :: Lude.Bool,
    -- | Contains the optional fields that are included in the inventory results.
    optionalFields :: Lude.Maybe [InventoryOptionalField],
    -- | The ID used to identify the inventory configuration.
    id :: Lude.Text,
    -- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
    filter :: Lude.Maybe InventoryFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryConfiguration' with the minimum fields required to make a request.
--
-- * 'includedObjectVersions' - Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
-- * 'destination' - Contains information about where to publish the inventory results.
-- * 'schedule' - Specifies the schedule for generating inventory results.
-- * 'isEnabled' - Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
-- * 'optionalFields' - Contains the optional fields that are included in the inventory results.
-- * 'id' - The ID used to identify the inventory configuration.
-- * 'filter' - Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
mkInventoryConfiguration ::
  -- | 'includedObjectVersions'
  InventoryIncludedObjectVersions ->
  -- | 'destination'
  InventoryDestination ->
  -- | 'schedule'
  InventorySchedule ->
  -- | 'isEnabled'
  Lude.Bool ->
  -- | 'id'
  Lude.Text ->
  InventoryConfiguration
mkInventoryConfiguration
  pIncludedObjectVersions_
  pDestination_
  pSchedule_
  pIsEnabled_
  pId_ =
    InventoryConfiguration'
      { includedObjectVersions =
          pIncludedObjectVersions_,
        destination = pDestination_,
        schedule = pSchedule_,
        isEnabled = pIsEnabled_,
        optionalFields = Lude.Nothing,
        id = pId_,
        filter = Lude.Nothing
      }

-- | Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
--
-- /Note:/ Consider using 'includedObjectVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIncludedObjectVersions :: Lens.Lens' InventoryConfiguration InventoryIncludedObjectVersions
icIncludedObjectVersions = Lens.lens (includedObjectVersions :: InventoryConfiguration -> InventoryIncludedObjectVersions) (\s a -> s {includedObjectVersions = a} :: InventoryConfiguration)
{-# DEPRECATED icIncludedObjectVersions "Use generic-lens or generic-optics with 'includedObjectVersions' instead." #-}

-- | Contains information about where to publish the inventory results.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icDestination :: Lens.Lens' InventoryConfiguration InventoryDestination
icDestination = Lens.lens (destination :: InventoryConfiguration -> InventoryDestination) (\s a -> s {destination = a} :: InventoryConfiguration)
{-# DEPRECATED icDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Specifies the schedule for generating inventory results.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSchedule :: Lens.Lens' InventoryConfiguration InventorySchedule
icSchedule = Lens.lens (schedule :: InventoryConfiguration -> InventorySchedule) (\s a -> s {schedule = a} :: InventoryConfiguration)
{-# DEPRECATED icSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
--
-- /Note:/ Consider using 'isEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIsEnabled :: Lens.Lens' InventoryConfiguration Lude.Bool
icIsEnabled = Lens.lens (isEnabled :: InventoryConfiguration -> Lude.Bool) (\s a -> s {isEnabled = a} :: InventoryConfiguration)
{-# DEPRECATED icIsEnabled "Use generic-lens or generic-optics with 'isEnabled' instead." #-}

-- | Contains the optional fields that are included in the inventory results.
--
-- /Note:/ Consider using 'optionalFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icOptionalFields :: Lens.Lens' InventoryConfiguration (Lude.Maybe [InventoryOptionalField])
icOptionalFields = Lens.lens (optionalFields :: InventoryConfiguration -> Lude.Maybe [InventoryOptionalField]) (\s a -> s {optionalFields = a} :: InventoryConfiguration)
{-# DEPRECATED icOptionalFields "Use generic-lens or generic-optics with 'optionalFields' instead." #-}

-- | The ID used to identify the inventory configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icId :: Lens.Lens' InventoryConfiguration Lude.Text
icId = Lens.lens (id :: InventoryConfiguration -> Lude.Text) (\s a -> s {id = a} :: InventoryConfiguration)
{-# DEPRECATED icId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icFilter :: Lens.Lens' InventoryConfiguration (Lude.Maybe InventoryFilter)
icFilter = Lens.lens (filter :: InventoryConfiguration -> Lude.Maybe InventoryFilter) (\s a -> s {filter = a} :: InventoryConfiguration)
{-# DEPRECATED icFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.FromXML InventoryConfiguration where
  parseXML x =
    InventoryConfiguration'
      Lude.<$> (x Lude..@ "IncludedObjectVersions")
      Lude.<*> (x Lude..@ "Destination")
      Lude.<*> (x Lude..@ "Schedule")
      Lude.<*> (x Lude..@ "IsEnabled")
      Lude.<*> ( x Lude..@? "OptionalFields" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Field")
               )
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@? "Filter")

instance Lude.ToXML InventoryConfiguration where
  toXML InventoryConfiguration' {..} =
    Lude.mconcat
      [ "IncludedObjectVersions" Lude.@= includedObjectVersions,
        "Destination" Lude.@= destination,
        "Schedule" Lude.@= schedule,
        "IsEnabled" Lude.@= isEnabled,
        "OptionalFields"
          Lude.@= Lude.toXML (Lude.toXMLList "Field" Lude.<$> optionalFields),
        "Id" Lude.@= id,
        "Filter" Lude.@= filter
      ]
