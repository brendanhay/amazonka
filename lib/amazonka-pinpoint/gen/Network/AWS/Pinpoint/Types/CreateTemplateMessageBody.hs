{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CreateTemplateMessageBody
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CreateTemplateMessageBody where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a request to create a message template.
--
--
--
-- /See:/ 'createTemplateMessageBody' smart constructor.
data CreateTemplateMessageBody = CreateTemplateMessageBody'
  { _ctmbRequestId ::
      !(Maybe Text),
    _ctmbARN :: !(Maybe Text),
    _ctmbMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTemplateMessageBody' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctmbRequestId' - The unique identifier for the request to create the message template.
--
-- * 'ctmbARN' - The Amazon Resource Name (ARN) of the message template that was created.
--
-- * 'ctmbMessage' - The message that's returned from the API for the request to create the message template.
createTemplateMessageBody ::
  CreateTemplateMessageBody
createTemplateMessageBody =
  CreateTemplateMessageBody'
    { _ctmbRequestId = Nothing,
      _ctmbARN = Nothing,
      _ctmbMessage = Nothing
    }

-- | The unique identifier for the request to create the message template.
ctmbRequestId :: Lens' CreateTemplateMessageBody (Maybe Text)
ctmbRequestId = lens _ctmbRequestId (\s a -> s {_ctmbRequestId = a})

-- | The Amazon Resource Name (ARN) of the message template that was created.
ctmbARN :: Lens' CreateTemplateMessageBody (Maybe Text)
ctmbARN = lens _ctmbARN (\s a -> s {_ctmbARN = a})

-- | The message that's returned from the API for the request to create the message template.
ctmbMessage :: Lens' CreateTemplateMessageBody (Maybe Text)
ctmbMessage = lens _ctmbMessage (\s a -> s {_ctmbMessage = a})

instance FromJSON CreateTemplateMessageBody where
  parseJSON =
    withObject
      "CreateTemplateMessageBody"
      ( \x ->
          CreateTemplateMessageBody'
            <$> (x .:? "RequestID") <*> (x .:? "Arn") <*> (x .:? "Message")
      )

instance Hashable CreateTemplateMessageBody

instance NFData CreateTemplateMessageBody
