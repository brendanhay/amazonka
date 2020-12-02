{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A description of the identity pool.
--
--
--
-- /See:/ 'identityPoolShortDescription' smart constructor.
data IdentityPoolShortDescription = IdentityPoolShortDescription'
  { _ipsdIdentityPoolId ::
      !(Maybe Text),
    _ipsdIdentityPoolName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IdentityPoolShortDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsdIdentityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- * 'ipsdIdentityPoolName' - A string that you provide.
identityPoolShortDescription ::
  IdentityPoolShortDescription
identityPoolShortDescription =
  IdentityPoolShortDescription'
    { _ipsdIdentityPoolId = Nothing,
      _ipsdIdentityPoolName = Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
ipsdIdentityPoolId :: Lens' IdentityPoolShortDescription (Maybe Text)
ipsdIdentityPoolId = lens _ipsdIdentityPoolId (\s a -> s {_ipsdIdentityPoolId = a})

-- | A string that you provide.
ipsdIdentityPoolName :: Lens' IdentityPoolShortDescription (Maybe Text)
ipsdIdentityPoolName = lens _ipsdIdentityPoolName (\s a -> s {_ipsdIdentityPoolName = a})

instance FromJSON IdentityPoolShortDescription where
  parseJSON =
    withObject
      "IdentityPoolShortDescription"
      ( \x ->
          IdentityPoolShortDescription'
            <$> (x .:? "IdentityPoolId") <*> (x .:? "IdentityPoolName")
      )

instance Hashable IdentityPoolShortDescription

instance NFData IdentityPoolShortDescription
