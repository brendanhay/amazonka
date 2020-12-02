{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Attachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.Attachment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An attachment to a case communication. The attachment consists of the file name and the content of the file.
--
--
--
-- /See:/ 'attachment' smart constructor.
data Attachment = Attachment'
  { _aData :: !(Maybe Base64),
    _aFileName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Attachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aData' - The content of the attachment file.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'aFileName' - The name of the attachment file.
attachment ::
  Attachment
attachment = Attachment' {_aData = Nothing, _aFileName = Nothing}

-- | The content of the attachment file.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
aData :: Lens' Attachment (Maybe ByteString)
aData = lens _aData (\s a -> s {_aData = a}) . mapping _Base64

-- | The name of the attachment file.
aFileName :: Lens' Attachment (Maybe Text)
aFileName = lens _aFileName (\s a -> s {_aFileName = a})

instance FromJSON Attachment where
  parseJSON =
    withObject
      "Attachment"
      (\x -> Attachment' <$> (x .:? "data") <*> (x .:? "fileName"))

instance Hashable Attachment

instance NFData Attachment

instance ToJSON Attachment where
  toJSON Attachment' {..} =
    object
      ( catMaybes
          [("data" .=) <$> _aData, ("fileName" .=) <$> _aFileName]
      )
