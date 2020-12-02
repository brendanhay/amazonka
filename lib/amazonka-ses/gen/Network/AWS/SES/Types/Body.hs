{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Body
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Body where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.Content

-- | Represents the body of the message. You can specify text, HTML, or both. If you use both, then the message should display correctly in the widest variety of email clients.
--
--
--
-- /See:/ 'body' smart constructor.
data Body = Body'
  { _bText :: !(Maybe Content),
    _bHTML :: !(Maybe Content)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Body' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bText' - The content of the message, in text format. Use this for text-based email clients, or clients on high-latency networks (such as mobile devices).
--
-- * 'bHTML' - The content of the message, in HTML format. Use this for email clients that can process HTML. You can include clickable links, formatted text, and much more in an HTML message.
body ::
  Body
body = Body' {_bText = Nothing, _bHTML = Nothing}

-- | The content of the message, in text format. Use this for text-based email clients, or clients on high-latency networks (such as mobile devices).
bText :: Lens' Body (Maybe Content)
bText = lens _bText (\s a -> s {_bText = a})

-- | The content of the message, in HTML format. Use this for email clients that can process HTML. You can include clickable links, formatted text, and much more in an HTML message.
bHTML :: Lens' Body (Maybe Content)
bHTML = lens _bHTML (\s a -> s {_bHTML = a})

instance Hashable Body

instance NFData Body

instance ToQuery Body where
  toQuery Body' {..} = mconcat ["Text" =: _bText, "Html" =: _bHTML]
