{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Dataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Dataset where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /See:/ 'dataset' smart constructor.
data Dataset = Dataset'
  { _dLastModifiedDate :: !(Maybe POSIX),
    _dNumRecords :: !(Maybe Integer),
    _dDataStorage :: !(Maybe Integer),
    _dDatasetName :: !(Maybe Text),
    _dCreationDate :: !(Maybe POSIX),
    _dLastModifiedBy :: !(Maybe Text),
    _dIdentityId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Dataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLastModifiedDate' - Date when the dataset was last modified.
--
-- * 'dNumRecords' - Number of records in this dataset.
--
-- * 'dDataStorage' - Total size in bytes of the records in this dataset.
--
-- * 'dDatasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- * 'dCreationDate' - Date on which the dataset was created.
--
-- * 'dLastModifiedBy' - The device that made the last change to this dataset.
--
-- * 'dIdentityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
dataset ::
  Dataset
dataset =
  Dataset'
    { _dLastModifiedDate = Nothing,
      _dNumRecords = Nothing,
      _dDataStorage = Nothing,
      _dDatasetName = Nothing,
      _dCreationDate = Nothing,
      _dLastModifiedBy = Nothing,
      _dIdentityId = Nothing
    }

-- | Date when the dataset was last modified.
dLastModifiedDate :: Lens' Dataset (Maybe UTCTime)
dLastModifiedDate = lens _dLastModifiedDate (\s a -> s {_dLastModifiedDate = a}) . mapping _Time

-- | Number of records in this dataset.
dNumRecords :: Lens' Dataset (Maybe Integer)
dNumRecords = lens _dNumRecords (\s a -> s {_dNumRecords = a})

-- | Total size in bytes of the records in this dataset.
dDataStorage :: Lens' Dataset (Maybe Integer)
dDataStorage = lens _dDataStorage (\s a -> s {_dDataStorage = a})

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
dDatasetName :: Lens' Dataset (Maybe Text)
dDatasetName = lens _dDatasetName (\s a -> s {_dDatasetName = a})

-- | Date on which the dataset was created.
dCreationDate :: Lens' Dataset (Maybe UTCTime)
dCreationDate = lens _dCreationDate (\s a -> s {_dCreationDate = a}) . mapping _Time

-- | The device that made the last change to this dataset.
dLastModifiedBy :: Lens' Dataset (Maybe Text)
dLastModifiedBy = lens _dLastModifiedBy (\s a -> s {_dLastModifiedBy = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
dIdentityId :: Lens' Dataset (Maybe Text)
dIdentityId = lens _dIdentityId (\s a -> s {_dIdentityId = a})

instance FromJSON Dataset where
  parseJSON =
    withObject
      "Dataset"
      ( \x ->
          Dataset'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "NumRecords")
            <*> (x .:? "DataStorage")
            <*> (x .:? "DatasetName")
            <*> (x .:? "CreationDate")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "IdentityId")
      )

instance Hashable Dataset

instance NFData Dataset
