{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccessKeyDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccessKeyDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the access keys.
--
--
--
-- /See:/ 'accessKeyDetails' smart constructor.
data AccessKeyDetails = AccessKeyDetails'
  { _akdPrincipalId ::
      !(Maybe Text),
    _akdUserName :: !(Maybe Text),
    _akdAccessKeyId :: !(Maybe Text),
    _akdUserType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessKeyDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akdPrincipalId' - The principal ID of the user.
--
-- * 'akdUserName' - The name of the user.
--
-- * 'akdAccessKeyId' - The access key ID of the user.
--
-- * 'akdUserType' - The type of the user.
accessKeyDetails ::
  AccessKeyDetails
accessKeyDetails =
  AccessKeyDetails'
    { _akdPrincipalId = Nothing,
      _akdUserName = Nothing,
      _akdAccessKeyId = Nothing,
      _akdUserType = Nothing
    }

-- | The principal ID of the user.
akdPrincipalId :: Lens' AccessKeyDetails (Maybe Text)
akdPrincipalId = lens _akdPrincipalId (\s a -> s {_akdPrincipalId = a})

-- | The name of the user.
akdUserName :: Lens' AccessKeyDetails (Maybe Text)
akdUserName = lens _akdUserName (\s a -> s {_akdUserName = a})

-- | The access key ID of the user.
akdAccessKeyId :: Lens' AccessKeyDetails (Maybe Text)
akdAccessKeyId = lens _akdAccessKeyId (\s a -> s {_akdAccessKeyId = a})

-- | The type of the user.
akdUserType :: Lens' AccessKeyDetails (Maybe Text)
akdUserType = lens _akdUserType (\s a -> s {_akdUserType = a})

instance FromJSON AccessKeyDetails where
  parseJSON =
    withObject
      "AccessKeyDetails"
      ( \x ->
          AccessKeyDetails'
            <$> (x .:? "principalId")
            <*> (x .:? "userName")
            <*> (x .:? "accessKeyId")
            <*> (x .:? "userType")
      )

instance Hashable AccessKeyDetails

instance NFData AccessKeyDetails
