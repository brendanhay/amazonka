{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceTagNotificationAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceTagNotificationAttribute where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the registered tag keys for the current Region.
--
--
--
-- /See:/ 'instanceTagNotificationAttribute' smart constructor.
data InstanceTagNotificationAttribute = InstanceTagNotificationAttribute'
  { _itnaIncludeAllTagsOfInstance ::
      !(Maybe Bool),
    _itnaInstanceTagKeys ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceTagNotificationAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itnaIncludeAllTagsOfInstance' - Indicates wheter all tag keys in the current Region are registered to appear in scheduled event notifications. @true@ indicates that all tag keys in the current Region are registered.
--
-- * 'itnaInstanceTagKeys' - The registered tag keys.
instanceTagNotificationAttribute ::
  InstanceTagNotificationAttribute
instanceTagNotificationAttribute =
  InstanceTagNotificationAttribute'
    { _itnaIncludeAllTagsOfInstance =
        Nothing,
      _itnaInstanceTagKeys = Nothing
    }

-- | Indicates wheter all tag keys in the current Region are registered to appear in scheduled event notifications. @true@ indicates that all tag keys in the current Region are registered.
itnaIncludeAllTagsOfInstance :: Lens' InstanceTagNotificationAttribute (Maybe Bool)
itnaIncludeAllTagsOfInstance = lens _itnaIncludeAllTagsOfInstance (\s a -> s {_itnaIncludeAllTagsOfInstance = a})

-- | The registered tag keys.
itnaInstanceTagKeys :: Lens' InstanceTagNotificationAttribute [Text]
itnaInstanceTagKeys = lens _itnaInstanceTagKeys (\s a -> s {_itnaInstanceTagKeys = a}) . _Default . _Coerce

instance FromXML InstanceTagNotificationAttribute where
  parseXML x =
    InstanceTagNotificationAttribute'
      <$> (x .@? "includeAllTagsOfInstance")
      <*> ( x .@? "instanceTagKeySet" .!@ mempty
              >>= may (parseXMLList "item")
          )

instance Hashable InstanceTagNotificationAttribute

instance NFData InstanceTagNotificationAttribute
