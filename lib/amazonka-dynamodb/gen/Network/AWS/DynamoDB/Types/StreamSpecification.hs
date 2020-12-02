{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.StreamSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.StreamSpecification where

import Network.AWS.DynamoDB.Types.StreamViewType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the DynamoDB Streams configuration for a table in DynamoDB.
--
--
--
-- /See:/ 'streamSpecification' smart constructor.
data StreamSpecification = StreamSpecification'
  { _ssStreamViewType ::
      !(Maybe StreamViewType),
    _ssStreamEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStreamViewType' - When an item in the table is modified, @StreamViewType@ determines what information is written to the stream for this table. Valid values for @StreamViewType@ are:     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
--
-- * 'ssStreamEnabled' - Indicates whether DynamoDB Streams is enabled (true) or disabled (false) on the table.
streamSpecification ::
  -- | 'ssStreamEnabled'
  Bool ->
  StreamSpecification
streamSpecification pStreamEnabled_ =
  StreamSpecification'
    { _ssStreamViewType = Nothing,
      _ssStreamEnabled = pStreamEnabled_
    }

-- | When an item in the table is modified, @StreamViewType@ determines what information is written to the stream for this table. Valid values for @StreamViewType@ are:     * @KEYS_ONLY@ - Only the key attributes of the modified item are written to the stream.     * @NEW_IMAGE@ - The entire item, as it appears after it was modified, is written to the stream.     * @OLD_IMAGE@ - The entire item, as it appeared before it was modified, is written to the stream.     * @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the item are written to the stream.
ssStreamViewType :: Lens' StreamSpecification (Maybe StreamViewType)
ssStreamViewType = lens _ssStreamViewType (\s a -> s {_ssStreamViewType = a})

-- | Indicates whether DynamoDB Streams is enabled (true) or disabled (false) on the table.
ssStreamEnabled :: Lens' StreamSpecification Bool
ssStreamEnabled = lens _ssStreamEnabled (\s a -> s {_ssStreamEnabled = a})

instance FromJSON StreamSpecification where
  parseJSON =
    withObject
      "StreamSpecification"
      ( \x ->
          StreamSpecification'
            <$> (x .:? "StreamViewType") <*> (x .: "StreamEnabled")
      )

instance Hashable StreamSpecification

instance NFData StreamSpecification

instance ToJSON StreamSpecification where
  toJSON StreamSpecification' {..} =
    object
      ( catMaybes
          [ ("StreamViewType" .=) <$> _ssStreamViewType,
            Just ("StreamEnabled" .= _ssStreamEnabled)
          ]
      )
