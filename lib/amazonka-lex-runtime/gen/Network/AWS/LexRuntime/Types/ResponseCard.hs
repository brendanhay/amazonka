{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ResponseCard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ResponseCard where

import Network.AWS.Lens
import Network.AWS.LexRuntime.Types.ContentType
import Network.AWS.LexRuntime.Types.GenericAttachment
import Network.AWS.Prelude

-- | If you configure a response card when creating your bots, Amazon Lex substitutes the session attributes and slot values that are available, and then returns it. The response card can also come from a Lambda function ( @dialogCodeHook@ and @fulfillmentActivity@ on an intent).
--
--
--
-- /See:/ 'responseCard' smart constructor.
data ResponseCard = ResponseCard'
  { _rcGenericAttachments ::
      !(Maybe [GenericAttachment]),
    _rcVersion :: !(Maybe Text),
    _rcContentType :: !(Maybe ContentType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResponseCard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcGenericAttachments' - An array of attachment objects representing options.
--
-- * 'rcVersion' - The version of the response card format.
--
-- * 'rcContentType' - The content type of the response.
responseCard ::
  ResponseCard
responseCard =
  ResponseCard'
    { _rcGenericAttachments = Nothing,
      _rcVersion = Nothing,
      _rcContentType = Nothing
    }

-- | An array of attachment objects representing options.
rcGenericAttachments :: Lens' ResponseCard [GenericAttachment]
rcGenericAttachments = lens _rcGenericAttachments (\s a -> s {_rcGenericAttachments = a}) . _Default . _Coerce

-- | The version of the response card format.
rcVersion :: Lens' ResponseCard (Maybe Text)
rcVersion = lens _rcVersion (\s a -> s {_rcVersion = a})

-- | The content type of the response.
rcContentType :: Lens' ResponseCard (Maybe ContentType)
rcContentType = lens _rcContentType (\s a -> s {_rcContentType = a})

instance FromJSON ResponseCard where
  parseJSON =
    withObject
      "ResponseCard"
      ( \x ->
          ResponseCard'
            <$> (x .:? "genericAttachments" .!= mempty)
            <*> (x .:? "version")
            <*> (x .:? "contentType")
      )

instance Hashable ResponseCard

instance NFData ResponseCard
