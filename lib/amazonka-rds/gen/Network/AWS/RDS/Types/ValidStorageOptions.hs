{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ValidStorageOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ValidStorageOptions where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.DoubleRange
import Network.AWS.RDS.Types.Range

-- | Information about valid modifications that you can make to your DB instance. Contains the result of a successful call to the @DescribeValidDBInstanceModifications@ action.
--
--
--
-- /See:/ 'validStorageOptions' smart constructor.
data ValidStorageOptions = ValidStorageOptions'
  { _vsoStorageSize ::
      !(Maybe [Range]),
    _vsoProvisionedIOPS :: !(Maybe [Range]),
    _vsoIOPSToStorageRatio :: !(Maybe [DoubleRange]),
    _vsoSupportsStorageAutoscaling :: !(Maybe Bool),
    _vsoStorageType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidStorageOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsoStorageSize' - The valid range of storage in gibibytes. For example, 100 to 16384.
--
-- * 'vsoProvisionedIOPS' - The valid range of provisioned IOPS. For example, 1000-20000.
--
-- * 'vsoIOPSToStorageRatio' - The valid range of Provisioned IOPS to gibibytes of storage multiplier. For example, 3-10, which means that provisioned IOPS can be between 3 and 10 times storage.
--
-- * 'vsoSupportsStorageAutoscaling' - Whether or not Amazon RDS can automatically scale storage for DB instances that use the new instance class.
--
-- * 'vsoStorageType' - The valid storage types for your DB instance. For example, gp2, io1.
validStorageOptions ::
  ValidStorageOptions
validStorageOptions =
  ValidStorageOptions'
    { _vsoStorageSize = Nothing,
      _vsoProvisionedIOPS = Nothing,
      _vsoIOPSToStorageRatio = Nothing,
      _vsoSupportsStorageAutoscaling = Nothing,
      _vsoStorageType = Nothing
    }

-- | The valid range of storage in gibibytes. For example, 100 to 16384.
vsoStorageSize :: Lens' ValidStorageOptions [Range]
vsoStorageSize = lens _vsoStorageSize (\s a -> s {_vsoStorageSize = a}) . _Default . _Coerce

-- | The valid range of provisioned IOPS. For example, 1000-20000.
vsoProvisionedIOPS :: Lens' ValidStorageOptions [Range]
vsoProvisionedIOPS = lens _vsoProvisionedIOPS (\s a -> s {_vsoProvisionedIOPS = a}) . _Default . _Coerce

-- | The valid range of Provisioned IOPS to gibibytes of storage multiplier. For example, 3-10, which means that provisioned IOPS can be between 3 and 10 times storage.
vsoIOPSToStorageRatio :: Lens' ValidStorageOptions [DoubleRange]
vsoIOPSToStorageRatio = lens _vsoIOPSToStorageRatio (\s a -> s {_vsoIOPSToStorageRatio = a}) . _Default . _Coerce

-- | Whether or not Amazon RDS can automatically scale storage for DB instances that use the new instance class.
vsoSupportsStorageAutoscaling :: Lens' ValidStorageOptions (Maybe Bool)
vsoSupportsStorageAutoscaling = lens _vsoSupportsStorageAutoscaling (\s a -> s {_vsoSupportsStorageAutoscaling = a})

-- | The valid storage types for your DB instance. For example, gp2, io1.
vsoStorageType :: Lens' ValidStorageOptions (Maybe Text)
vsoStorageType = lens _vsoStorageType (\s a -> s {_vsoStorageType = a})

instance FromXML ValidStorageOptions where
  parseXML x =
    ValidStorageOptions'
      <$> (x .@? "StorageSize" .!@ mempty >>= may (parseXMLList "Range"))
      <*> (x .@? "ProvisionedIops" .!@ mempty >>= may (parseXMLList "Range"))
      <*> ( x .@? "IopsToStorageRatio" .!@ mempty
              >>= may (parseXMLList "DoubleRange")
          )
      <*> (x .@? "SupportsStorageAutoscaling")
      <*> (x .@? "StorageType")

instance Hashable ValidStorageOptions

instance NFData ValidStorageOptions
