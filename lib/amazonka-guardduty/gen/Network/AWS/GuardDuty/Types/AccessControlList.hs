{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccessControlList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccessControlList where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the current access control policies for the bucket.
--
--
--
-- /See:/ 'accessControlList' smart constructor.
data AccessControlList = AccessControlList'
  { _aclAllowsPublicWriteAccess ::
      !(Maybe Bool),
    _aclAllowsPublicReadAccess :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessControlList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aclAllowsPublicWriteAccess' - A value that indicates whether public write access for the bucket is enabled through an Access Control List (ACL).
--
-- * 'aclAllowsPublicReadAccess' - A value that indicates whether public read access for the bucket is enabled through an Access Control List (ACL).
accessControlList ::
  AccessControlList
accessControlList =
  AccessControlList'
    { _aclAllowsPublicWriteAccess = Nothing,
      _aclAllowsPublicReadAccess = Nothing
    }

-- | A value that indicates whether public write access for the bucket is enabled through an Access Control List (ACL).
aclAllowsPublicWriteAccess :: Lens' AccessControlList (Maybe Bool)
aclAllowsPublicWriteAccess = lens _aclAllowsPublicWriteAccess (\s a -> s {_aclAllowsPublicWriteAccess = a})

-- | A value that indicates whether public read access for the bucket is enabled through an Access Control List (ACL).
aclAllowsPublicReadAccess :: Lens' AccessControlList (Maybe Bool)
aclAllowsPublicReadAccess = lens _aclAllowsPublicReadAccess (\s a -> s {_aclAllowsPublicReadAccess = a})

instance FromJSON AccessControlList where
  parseJSON =
    withObject
      "AccessControlList"
      ( \x ->
          AccessControlList'
            <$> (x .:? "allowsPublicWriteAccess")
            <*> (x .:? "allowsPublicReadAccess")
      )

instance Hashable AccessControlList

instance NFData AccessControlList
