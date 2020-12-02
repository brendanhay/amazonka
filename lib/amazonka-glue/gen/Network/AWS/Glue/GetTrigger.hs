{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a trigger.
module Network.AWS.Glue.GetTrigger
  ( -- * Creating a Request
    getTrigger,
    GetTrigger,

    -- * Request Lenses
    gtName,

    -- * Destructuring the Response
    getTriggerResponse,
    GetTriggerResponse,

    -- * Response Lenses
    getrsTrigger,
    getrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTrigger' smart constructor.
newtype GetTrigger = GetTrigger' {_gtName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtName' - The name of the trigger to retrieve.
getTrigger ::
  -- | 'gtName'
  Text ->
  GetTrigger
getTrigger pName_ = GetTrigger' {_gtName = pName_}

-- | The name of the trigger to retrieve.
gtName :: Lens' GetTrigger Text
gtName = lens _gtName (\s a -> s {_gtName = a})

instance AWSRequest GetTrigger where
  type Rs GetTrigger = GetTriggerResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetTriggerResponse' <$> (x .?> "Trigger") <*> (pure (fromEnum s))
      )

instance Hashable GetTrigger

instance NFData GetTrigger

instance ToHeaders GetTrigger where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetTrigger" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetTrigger where
  toJSON GetTrigger' {..} =
    object (catMaybes [Just ("Name" .= _gtName)])

instance ToPath GetTrigger where
  toPath = const "/"

instance ToQuery GetTrigger where
  toQuery = const mempty

-- | /See:/ 'getTriggerResponse' smart constructor.
data GetTriggerResponse = GetTriggerResponse'
  { _getrsTrigger ::
      !(Maybe Trigger),
    _getrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTriggerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsTrigger' - The requested trigger definition.
--
-- * 'getrsResponseStatus' - -- | The response status code.
getTriggerResponse ::
  -- | 'getrsResponseStatus'
  Int ->
  GetTriggerResponse
getTriggerResponse pResponseStatus_ =
  GetTriggerResponse'
    { _getrsTrigger = Nothing,
      _getrsResponseStatus = pResponseStatus_
    }

-- | The requested trigger definition.
getrsTrigger :: Lens' GetTriggerResponse (Maybe Trigger)
getrsTrigger = lens _getrsTrigger (\s a -> s {_getrsTrigger = a})

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetTriggerResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\s a -> s {_getrsResponseStatus = a})

instance NFData GetTriggerResponse
