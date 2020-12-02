{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogImportStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure containing migration status information.
--
--
--
-- /See:/ 'catalogImportStatus' smart constructor.
data CatalogImportStatus = CatalogImportStatus'
  { _cisImportedBy ::
      !(Maybe Text),
    _cisImportTime :: !(Maybe POSIX),
    _cisImportCompleted :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CatalogImportStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisImportedBy' - The name of the person who initiated the migration.
--
-- * 'cisImportTime' - The time that the migration was started.
--
-- * 'cisImportCompleted' - @True@ if the migration has completed, or @False@ otherwise.
catalogImportStatus ::
  CatalogImportStatus
catalogImportStatus =
  CatalogImportStatus'
    { _cisImportedBy = Nothing,
      _cisImportTime = Nothing,
      _cisImportCompleted = Nothing
    }

-- | The name of the person who initiated the migration.
cisImportedBy :: Lens' CatalogImportStatus (Maybe Text)
cisImportedBy = lens _cisImportedBy (\s a -> s {_cisImportedBy = a})

-- | The time that the migration was started.
cisImportTime :: Lens' CatalogImportStatus (Maybe UTCTime)
cisImportTime = lens _cisImportTime (\s a -> s {_cisImportTime = a}) . mapping _Time

-- | @True@ if the migration has completed, or @False@ otherwise.
cisImportCompleted :: Lens' CatalogImportStatus (Maybe Bool)
cisImportCompleted = lens _cisImportCompleted (\s a -> s {_cisImportCompleted = a})

instance FromJSON CatalogImportStatus where
  parseJSON =
    withObject
      "CatalogImportStatus"
      ( \x ->
          CatalogImportStatus'
            <$> (x .:? "ImportedBy")
            <*> (x .:? "ImportTime")
            <*> (x .:? "ImportCompleted")
      )

instance Hashable CatalogImportStatus

instance NFData CatalogImportStatus
