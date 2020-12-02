{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.FixedResponseActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.FixedResponseActionConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an action that returns a custom HTTP response.
--
--
--
-- /See:/ 'fixedResponseActionConfig' smart constructor.
data FixedResponseActionConfig = FixedResponseActionConfig'
  { _fracMessageBody ::
      !(Maybe Text),
    _fracContentType :: !(Maybe Text),
    _fracStatusCode :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FixedResponseActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fracMessageBody' - The message.
--
-- * 'fracContentType' - The content type. Valid Values: text/plain | text/css | text/html | application/javascript | application/json
--
-- * 'fracStatusCode' - The HTTP response code (2XX, 4XX, or 5XX).
fixedResponseActionConfig ::
  -- | 'fracStatusCode'
  Text ->
  FixedResponseActionConfig
fixedResponseActionConfig pStatusCode_ =
  FixedResponseActionConfig'
    { _fracMessageBody = Nothing,
      _fracContentType = Nothing,
      _fracStatusCode = pStatusCode_
    }

-- | The message.
fracMessageBody :: Lens' FixedResponseActionConfig (Maybe Text)
fracMessageBody = lens _fracMessageBody (\s a -> s {_fracMessageBody = a})

-- | The content type. Valid Values: text/plain | text/css | text/html | application/javascript | application/json
fracContentType :: Lens' FixedResponseActionConfig (Maybe Text)
fracContentType = lens _fracContentType (\s a -> s {_fracContentType = a})

-- | The HTTP response code (2XX, 4XX, or 5XX).
fracStatusCode :: Lens' FixedResponseActionConfig Text
fracStatusCode = lens _fracStatusCode (\s a -> s {_fracStatusCode = a})

instance FromXML FixedResponseActionConfig where
  parseXML x =
    FixedResponseActionConfig'
      <$> (x .@? "MessageBody")
      <*> (x .@? "ContentType")
      <*> (x .@ "StatusCode")

instance Hashable FixedResponseActionConfig

instance NFData FixedResponseActionConfig

instance ToQuery FixedResponseActionConfig where
  toQuery FixedResponseActionConfig' {..} =
    mconcat
      [ "MessageBody" =: _fracMessageBody,
        "ContentType" =: _fracContentType,
        "StatusCode" =: _fracStatusCode
      ]
