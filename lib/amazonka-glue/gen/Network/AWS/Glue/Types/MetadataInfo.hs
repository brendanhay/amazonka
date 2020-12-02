{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MetadataInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MetadataInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure containing metadata information for a schema version.
--
--
--
-- /See:/ 'metadataInfo' smart constructor.
data MetadataInfo = MetadataInfo'
  { _miCreatedTime :: !(Maybe Text),
    _miMetadataValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetadataInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miCreatedTime' - The time at which the entry was created.
--
-- * 'miMetadataValue' - The metadata key’s corresponding value.
metadataInfo ::
  MetadataInfo
metadataInfo =
  MetadataInfo'
    { _miCreatedTime = Nothing,
      _miMetadataValue = Nothing
    }

-- | The time at which the entry was created.
miCreatedTime :: Lens' MetadataInfo (Maybe Text)
miCreatedTime = lens _miCreatedTime (\s a -> s {_miCreatedTime = a})

-- | The metadata key’s corresponding value.
miMetadataValue :: Lens' MetadataInfo (Maybe Text)
miMetadataValue = lens _miMetadataValue (\s a -> s {_miMetadataValue = a})

instance FromJSON MetadataInfo where
  parseJSON =
    withObject
      "MetadataInfo"
      ( \x ->
          MetadataInfo'
            <$> (x .:? "CreatedTime") <*> (x .:? "MetadataValue")
      )

instance Hashable MetadataInfo

instance NFData MetadataInfo
