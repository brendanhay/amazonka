{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RecordsEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RecordsEvent where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | The container for the records event.
--
--
--
-- /See:/ 'recordsEvent' smart constructor.
newtype RecordsEvent = RecordsEvent' {_rePayload :: Maybe Base64}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecordsEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rePayload' - The byte array of partial, one or more result records.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
recordsEvent ::
  RecordsEvent
recordsEvent = RecordsEvent' {_rePayload = Nothing}

-- | The byte array of partial, one or more result records.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rePayload :: Lens' RecordsEvent (Maybe ByteString)
rePayload = lens _rePayload (\s a -> s {_rePayload = a}) . mapping _Base64

instance FromXML RecordsEvent where
  parseXML x = RecordsEvent' <$> (x .@? "Payload")

instance Hashable RecordsEvent

instance NFData RecordsEvent
