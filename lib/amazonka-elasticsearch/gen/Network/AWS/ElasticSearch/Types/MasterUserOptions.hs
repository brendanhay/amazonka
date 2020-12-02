{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.MasterUserOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.MasterUserOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Credentials for the master user: username and password, ARN, or both.
--
--
--
-- /See:/ 'masterUserOptions' smart constructor.
data MasterUserOptions = MasterUserOptions'
  { _muoMasterUserPassword ::
      !(Maybe (Sensitive Text)),
    _muoMasterUserName :: !(Maybe (Sensitive Text)),
    _muoMasterUserARN :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MasterUserOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'muoMasterUserPassword' - The master user's password, which is stored in the Amazon Elasticsearch Service domain's internal database.
--
-- * 'muoMasterUserName' - The master user's username, which is stored in the Amazon Elasticsearch Service domain's internal database.
--
-- * 'muoMasterUserARN' - ARN for the master user (if IAM is enabled).
masterUserOptions ::
  MasterUserOptions
masterUserOptions =
  MasterUserOptions'
    { _muoMasterUserPassword = Nothing,
      _muoMasterUserName = Nothing,
      _muoMasterUserARN = Nothing
    }

-- | The master user's password, which is stored in the Amazon Elasticsearch Service domain's internal database.
muoMasterUserPassword :: Lens' MasterUserOptions (Maybe Text)
muoMasterUserPassword = lens _muoMasterUserPassword (\s a -> s {_muoMasterUserPassword = a}) . mapping _Sensitive

-- | The master user's username, which is stored in the Amazon Elasticsearch Service domain's internal database.
muoMasterUserName :: Lens' MasterUserOptions (Maybe Text)
muoMasterUserName = lens _muoMasterUserName (\s a -> s {_muoMasterUserName = a}) . mapping _Sensitive

-- | ARN for the master user (if IAM is enabled).
muoMasterUserARN :: Lens' MasterUserOptions (Maybe Text)
muoMasterUserARN = lens _muoMasterUserARN (\s a -> s {_muoMasterUserARN = a})

instance Hashable MasterUserOptions

instance NFData MasterUserOptions

instance ToJSON MasterUserOptions where
  toJSON MasterUserOptions' {..} =
    object
      ( catMaybes
          [ ("MasterUserPassword" .=) <$> _muoMasterUserPassword,
            ("MasterUserName" .=) <$> _muoMasterUserName,
            ("MasterUserARN" .=) <$> _muoMasterUserARN
          ]
      )
