{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.IdentityUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.IdentityUsage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Usage information for the identity.
--
-- /See:/ 'identityUsage' smart constructor.
data IdentityUsage = IdentityUsage'
  { _iuLastModifiedDate ::
      !(Maybe POSIX),
    _iuIdentityPoolId :: !(Maybe Text),
    _iuDatasetCount :: !(Maybe Int),
    _iuDataStorage :: !(Maybe Integer),
    _iuIdentityId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IdentityUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iuLastModifiedDate' - Date on which the identity was last modified.
--
-- * 'iuIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'iuDatasetCount' - Number of datasets for the identity.
--
-- * 'iuDataStorage' - Total data storage for this identity.
--
-- * 'iuIdentityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
identityUsage ::
  IdentityUsage
identityUsage =
  IdentityUsage'
    { _iuLastModifiedDate = Nothing,
      _iuIdentityPoolId = Nothing,
      _iuDatasetCount = Nothing,
      _iuDataStorage = Nothing,
      _iuIdentityId = Nothing
    }

-- | Date on which the identity was last modified.
iuLastModifiedDate :: Lens' IdentityUsage (Maybe UTCTime)
iuLastModifiedDate = lens _iuLastModifiedDate (\s a -> s {_iuLastModifiedDate = a}) . mapping _Time

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
iuIdentityPoolId :: Lens' IdentityUsage (Maybe Text)
iuIdentityPoolId = lens _iuIdentityPoolId (\s a -> s {_iuIdentityPoolId = a})

-- | Number of datasets for the identity.
iuDatasetCount :: Lens' IdentityUsage (Maybe Int)
iuDatasetCount = lens _iuDatasetCount (\s a -> s {_iuDatasetCount = a})

-- | Total data storage for this identity.
iuDataStorage :: Lens' IdentityUsage (Maybe Integer)
iuDataStorage = lens _iuDataStorage (\s a -> s {_iuDataStorage = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
iuIdentityId :: Lens' IdentityUsage (Maybe Text)
iuIdentityId = lens _iuIdentityId (\s a -> s {_iuIdentityId = a})

instance FromJSON IdentityUsage where
  parseJSON =
    withObject
      "IdentityUsage"
      ( \x ->
          IdentityUsage'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "IdentityPoolId")
            <*> (x .:? "DatasetCount")
            <*> (x .:? "DataStorage")
            <*> (x .:? "IdentityId")
      )

instance Hashable IdentityUsage

instance NFData IdentityUsage
