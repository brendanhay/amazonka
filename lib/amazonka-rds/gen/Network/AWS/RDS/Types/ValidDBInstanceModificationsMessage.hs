{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.AvailableProcessorFeature
import Network.AWS.RDS.Types.ValidStorageOptions

-- | Information about valid modifications that you can make to your DB instance. Contains the result of a successful call to the @DescribeValidDBInstanceModifications@ action. You can use this information when you call @ModifyDBInstance@ .
--
--
--
-- /See:/ 'validDBInstanceModificationsMessage' smart constructor.
data ValidDBInstanceModificationsMessage = ValidDBInstanceModificationsMessage'
  { _vdimmValidProcessorFeatures ::
      !( Maybe
           [AvailableProcessorFeature]
       ),
    _vdimmStorage ::
      !( Maybe
           [ValidStorageOptions]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidDBInstanceModificationsMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdimmValidProcessorFeatures' - Valid processor features for your DB instance.
--
-- * 'vdimmStorage' - Valid storage options for your DB instance.
validDBInstanceModificationsMessage ::
  ValidDBInstanceModificationsMessage
validDBInstanceModificationsMessage =
  ValidDBInstanceModificationsMessage'
    { _vdimmValidProcessorFeatures =
        Nothing,
      _vdimmStorage = Nothing
    }

-- | Valid processor features for your DB instance.
vdimmValidProcessorFeatures :: Lens' ValidDBInstanceModificationsMessage [AvailableProcessorFeature]
vdimmValidProcessorFeatures = lens _vdimmValidProcessorFeatures (\s a -> s {_vdimmValidProcessorFeatures = a}) . _Default . _Coerce

-- | Valid storage options for your DB instance.
vdimmStorage :: Lens' ValidDBInstanceModificationsMessage [ValidStorageOptions]
vdimmStorage = lens _vdimmStorage (\s a -> s {_vdimmStorage = a}) . _Default . _Coerce

instance FromXML ValidDBInstanceModificationsMessage where
  parseXML x =
    ValidDBInstanceModificationsMessage'
      <$> ( x .@? "ValidProcessorFeatures" .!@ mempty
              >>= may (parseXMLList "AvailableProcessorFeature")
          )
      <*> ( x .@? "Storage" .!@ mempty
              >>= may (parseXMLList "ValidStorageOptions")
          )

instance Hashable ValidDBInstanceModificationsMessage

instance NFData ValidDBInstanceModificationsMessage
