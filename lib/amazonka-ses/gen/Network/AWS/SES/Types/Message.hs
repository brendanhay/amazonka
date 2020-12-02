{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Message where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.Body
import Network.AWS.SES.Types.Content

-- | Represents the message to be sent, composed of a subject and a body.
--
--
--
-- /See:/ 'message' smart constructor.
data Message = Message' {_mSubject :: !Content, _mBody :: !Body}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mSubject' - The subject of the message: A short summary of the content, which will appear in the recipient's inbox.
--
-- * 'mBody' - The message body.
message ::
  -- | 'mSubject'
  Content ->
  -- | 'mBody'
  Body ->
  Message
message pSubject_ pBody_ =
  Message' {_mSubject = pSubject_, _mBody = pBody_}

-- | The subject of the message: A short summary of the content, which will appear in the recipient's inbox.
mSubject :: Lens' Message Content
mSubject = lens _mSubject (\s a -> s {_mSubject = a})

-- | The message body.
mBody :: Lens' Message Body
mBody = lens _mBody (\s a -> s {_mBody = a})

instance Hashable Message

instance NFData Message

instance ToQuery Message where
  toQuery Message' {..} =
    mconcat ["Subject" =: _mSubject, "Body" =: _mBody]
