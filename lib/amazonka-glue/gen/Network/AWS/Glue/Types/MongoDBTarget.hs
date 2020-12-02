{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MongoDBTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MongoDBTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an Amazon DocumentDB or MongoDB data store to crawl.
--
--
--
-- /See:/ 'mongoDBTarget' smart constructor.
data MongoDBTarget = MongoDBTarget'
  { _mdtPath :: !(Maybe Text),
    _mdtConnectionName :: !(Maybe Text),
    _mdtScanAll :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MongoDBTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdtPath' - The path of the Amazon DocumentDB or MongoDB target (database/collection).
--
-- * 'mdtConnectionName' - The name of the connection to use to connect to the Amazon DocumentDB or MongoDB target.
--
-- * 'mdtScanAll' - Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table. A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
mongoDBTarget ::
  MongoDBTarget
mongoDBTarget =
  MongoDBTarget'
    { _mdtPath = Nothing,
      _mdtConnectionName = Nothing,
      _mdtScanAll = Nothing
    }

-- | The path of the Amazon DocumentDB or MongoDB target (database/collection).
mdtPath :: Lens' MongoDBTarget (Maybe Text)
mdtPath = lens _mdtPath (\s a -> s {_mdtPath = a})

-- | The name of the connection to use to connect to the Amazon DocumentDB or MongoDB target.
mdtConnectionName :: Lens' MongoDBTarget (Maybe Text)
mdtConnectionName = lens _mdtConnectionName (\s a -> s {_mdtConnectionName = a})

-- | Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table. A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
mdtScanAll :: Lens' MongoDBTarget (Maybe Bool)
mdtScanAll = lens _mdtScanAll (\s a -> s {_mdtScanAll = a})

instance FromJSON MongoDBTarget where
  parseJSON =
    withObject
      "MongoDBTarget"
      ( \x ->
          MongoDBTarget'
            <$> (x .:? "Path") <*> (x .:? "ConnectionName") <*> (x .:? "ScanAll")
      )

instance Hashable MongoDBTarget

instance NFData MongoDBTarget

instance ToJSON MongoDBTarget where
  toJSON MongoDBTarget' {..} =
    object
      ( catMaybes
          [ ("Path" .=) <$> _mdtPath,
            ("ConnectionName" .=) <$> _mdtConnectionName,
            ("ScanAll" .=) <$> _mdtScanAll
          ]
      )
