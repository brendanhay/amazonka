{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RawEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RawEmail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the contents of an email message, represented as a raw MIME message.
--
--
--
-- /See:/ 'rawEmail' smart constructor.
newtype RawEmail = RawEmail' {_reData :: Maybe Base64}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RawEmail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reData' - The email message, represented as a raw MIME message. The entire message must be base64 encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rawEmail ::
  RawEmail
rawEmail = RawEmail' {_reData = Nothing}

-- | The email message, represented as a raw MIME message. The entire message must be base64 encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
reData :: Lens' RawEmail (Maybe ByteString)
reData = lens _reData (\s a -> s {_reData = a}) . mapping _Base64

instance Hashable RawEmail

instance NFData RawEmail

instance ToJSON RawEmail where
  toJSON RawEmail' {..} = object (catMaybes [("Data" .=) <$> _reData])
