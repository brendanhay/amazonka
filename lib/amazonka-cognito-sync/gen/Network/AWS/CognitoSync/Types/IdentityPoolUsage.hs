{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.IdentityPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.IdentityPoolUsage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Usage information for the identity pool.
--
-- /See:/ 'identityPoolUsage' smart constructor.
data IdentityPoolUsage = IdentityPoolUsage'
  { _ipuLastModifiedDate ::
      !(Maybe POSIX),
    _ipuIdentityPoolId :: !(Maybe Text),
    _ipuDataStorage :: !(Maybe Integer),
    _ipuSyncSessionsCount :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IdentityPoolUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipuLastModifiedDate' - Date on which the identity pool was last modified.
--
-- * 'ipuIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'ipuDataStorage' - Data storage information for the identity pool.
--
-- * 'ipuSyncSessionsCount' - Number of sync sessions for the identity pool.
identityPoolUsage ::
  IdentityPoolUsage
identityPoolUsage =
  IdentityPoolUsage'
    { _ipuLastModifiedDate = Nothing,
      _ipuIdentityPoolId = Nothing,
      _ipuDataStorage = Nothing,
      _ipuSyncSessionsCount = Nothing
    }

-- | Date on which the identity pool was last modified.
ipuLastModifiedDate :: Lens' IdentityPoolUsage (Maybe UTCTime)
ipuLastModifiedDate = lens _ipuLastModifiedDate (\s a -> s {_ipuLastModifiedDate = a}) . mapping _Time

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
ipuIdentityPoolId :: Lens' IdentityPoolUsage (Maybe Text)
ipuIdentityPoolId = lens _ipuIdentityPoolId (\s a -> s {_ipuIdentityPoolId = a})

-- | Data storage information for the identity pool.
ipuDataStorage :: Lens' IdentityPoolUsage (Maybe Integer)
ipuDataStorage = lens _ipuDataStorage (\s a -> s {_ipuDataStorage = a})

-- | Number of sync sessions for the identity pool.
ipuSyncSessionsCount :: Lens' IdentityPoolUsage (Maybe Integer)
ipuSyncSessionsCount = lens _ipuSyncSessionsCount (\s a -> s {_ipuSyncSessionsCount = a})

instance FromJSON IdentityPoolUsage where
  parseJSON =
    withObject
      "IdentityPoolUsage"
      ( \x ->
          IdentityPoolUsage'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "IdentityPoolId")
            <*> (x .:? "DataStorage")
            <*> (x .:? "SyncSessionsCount")
      )

instance Hashable IdentityPoolUsage

instance NFData IdentityPoolUsage
