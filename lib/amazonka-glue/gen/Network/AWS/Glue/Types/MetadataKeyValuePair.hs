{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MetadataKeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MetadataKeyValuePair where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure containing a key value pair for metadata.
--
--
--
-- /See:/ 'metadataKeyValuePair' smart constructor.
data MetadataKeyValuePair = MetadataKeyValuePair'
  { _mkvpMetadataKey ::
      !(Maybe Text),
    _mkvpMetadataValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetadataKeyValuePair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mkvpMetadataKey' - A metadata key.
--
-- * 'mkvpMetadataValue' - A metadata key’s corresponding value.
metadataKeyValuePair ::
  MetadataKeyValuePair
metadataKeyValuePair =
  MetadataKeyValuePair'
    { _mkvpMetadataKey = Nothing,
      _mkvpMetadataValue = Nothing
    }

-- | A metadata key.
mkvpMetadataKey :: Lens' MetadataKeyValuePair (Maybe Text)
mkvpMetadataKey = lens _mkvpMetadataKey (\s a -> s {_mkvpMetadataKey = a})

-- | A metadata key’s corresponding value.
mkvpMetadataValue :: Lens' MetadataKeyValuePair (Maybe Text)
mkvpMetadataValue = lens _mkvpMetadataValue (\s a -> s {_mkvpMetadataValue = a})

instance Hashable MetadataKeyValuePair

instance NFData MetadataKeyValuePair

instance ToJSON MetadataKeyValuePair where
  toJSON MetadataKeyValuePair' {..} =
    object
      ( catMaybes
          [ ("MetadataKey" .=) <$> _mkvpMetadataKey,
            ("MetadataValue" .=) <$> _mkvpMetadataValue
          ]
      )
