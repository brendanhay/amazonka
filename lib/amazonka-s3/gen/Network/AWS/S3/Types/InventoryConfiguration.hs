{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryDestination
import Network.AWS.S3.Types.InventoryFilter
import Network.AWS.S3.Types.InventoryIncludedObjectVersions
import Network.AWS.S3.Types.InventoryOptionalField
import Network.AWS.S3.Types.InventorySchedule

-- | Specifies the inventory configuration for an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETInventoryConfig.html GET Bucket inventory> in the /Amazon Simple Storage Service API Reference/ .
--
--
--
-- /See:/ 'inventoryConfiguration' smart constructor.
data InventoryConfiguration = InventoryConfiguration'
  { _icOptionalFields ::
      !(Maybe [InventoryOptionalField]),
    _icFilter :: !(Maybe InventoryFilter),
    _icDestination :: !InventoryDestination,
    _icIsEnabled :: !Bool,
    _icId :: !Text,
    _icIncludedObjectVersions ::
      !InventoryIncludedObjectVersions,
    _icSchedule :: !InventorySchedule
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icOptionalFields' - Contains the optional fields that are included in the inventory results.
--
-- * 'icFilter' - Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
-- * 'icDestination' - Contains information about where to publish the inventory results.
--
-- * 'icIsEnabled' - Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
--
-- * 'icId' - The ID used to identify the inventory configuration.
--
-- * 'icIncludedObjectVersions' - Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
--
-- * 'icSchedule' - Specifies the schedule for generating inventory results.
inventoryConfiguration ::
  -- | 'icDestination'
  InventoryDestination ->
  -- | 'icIsEnabled'
  Bool ->
  -- | 'icId'
  Text ->
  -- | 'icIncludedObjectVersions'
  InventoryIncludedObjectVersions ->
  -- | 'icSchedule'
  InventorySchedule ->
  InventoryConfiguration
inventoryConfiguration
  pDestination_
  pIsEnabled_
  pId_
  pIncludedObjectVersions_
  pSchedule_ =
    InventoryConfiguration'
      { _icOptionalFields = Nothing,
        _icFilter = Nothing,
        _icDestination = pDestination_,
        _icIsEnabled = pIsEnabled_,
        _icId = pId_,
        _icIncludedObjectVersions = pIncludedObjectVersions_,
        _icSchedule = pSchedule_
      }

-- | Contains the optional fields that are included in the inventory results.
icOptionalFields :: Lens' InventoryConfiguration [InventoryOptionalField]
icOptionalFields = lens _icOptionalFields (\s a -> s {_icOptionalFields = a}) . _Default . _Coerce

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
icFilter :: Lens' InventoryConfiguration (Maybe InventoryFilter)
icFilter = lens _icFilter (\s a -> s {_icFilter = a})

-- | Contains information about where to publish the inventory results.
icDestination :: Lens' InventoryConfiguration InventoryDestination
icDestination = lens _icDestination (\s a -> s {_icDestination = a})

-- | Specifies whether the inventory is enabled or disabled. If set to @True@ , an inventory list is generated. If set to @False@ , no inventory list is generated.
icIsEnabled :: Lens' InventoryConfiguration Bool
icIsEnabled = lens _icIsEnabled (\s a -> s {_icIsEnabled = a})

-- | The ID used to identify the inventory configuration.
icId :: Lens' InventoryConfiguration Text
icId = lens _icId (\s a -> s {_icId = a})

-- | Object versions to include in the inventory list. If set to @All@ , the list includes all the object versions, which adds the version-related fields @VersionId@ , @IsLatest@ , and @DeleteMarker@ to the list. If set to @Current@ , the list does not contain these version-related fields.
icIncludedObjectVersions :: Lens' InventoryConfiguration InventoryIncludedObjectVersions
icIncludedObjectVersions = lens _icIncludedObjectVersions (\s a -> s {_icIncludedObjectVersions = a})

-- | Specifies the schedule for generating inventory results.
icSchedule :: Lens' InventoryConfiguration InventorySchedule
icSchedule = lens _icSchedule (\s a -> s {_icSchedule = a})

instance FromXML InventoryConfiguration where
  parseXML x =
    InventoryConfiguration'
      <$> (x .@? "OptionalFields" .!@ mempty >>= may (parseXMLList "Field"))
      <*> (x .@? "Filter")
      <*> (x .@ "Destination")
      <*> (x .@ "IsEnabled")
      <*> (x .@ "Id")
      <*> (x .@ "IncludedObjectVersions")
      <*> (x .@ "Schedule")

instance Hashable InventoryConfiguration

instance NFData InventoryConfiguration

instance ToXML InventoryConfiguration where
  toXML InventoryConfiguration' {..} =
    mconcat
      [ "OptionalFields"
          @= toXML (toXMLList "Field" <$> _icOptionalFields),
        "Filter" @= _icFilter,
        "Destination" @= _icDestination,
        "IsEnabled" @= _icIsEnabled,
        "Id" @= _icId,
        "IncludedObjectVersions" @= _icIncludedObjectVersions,
        "Schedule" @= _icSchedule
      ]
