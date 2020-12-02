{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Message where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a message.
--
--
--
-- /See:/ 'message' smart constructor.
data Message = Message' {_mMessageId :: !Text, _mPayload :: !Base64}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMessageId' - The ID you want to assign to the message. Each @messageId@ must be unique within each batch sent.
--
-- * 'mPayload' - The payload of the message. This can be a JSON string or a base64-encoded string representing binary data, in which case you must decode it by means of a pipeline activity.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
message ::
  -- | 'mMessageId'
  Text ->
  -- | 'mPayload'
  ByteString ->
  Message
message pMessageId_ pPayload_ =
  Message'
    { _mMessageId = pMessageId_,
      _mPayload = _Base64 # pPayload_
    }

-- | The ID you want to assign to the message. Each @messageId@ must be unique within each batch sent.
mMessageId :: Lens' Message Text
mMessageId = lens _mMessageId (\s a -> s {_mMessageId = a})

-- | The payload of the message. This can be a JSON string or a base64-encoded string representing binary data, in which case you must decode it by means of a pipeline activity.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mPayload :: Lens' Message ByteString
mPayload = lens _mPayload (\s a -> s {_mPayload = a}) . _Base64

instance Hashable Message

instance NFData Message

instance ToJSON Message where
  toJSON Message' {..} =
    object
      ( catMaybes
          [Just ("messageId" .= _mMessageId), Just ("payload" .= _mPayload)]
      )
