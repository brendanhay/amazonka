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
-- Module      : Network.AWS.RDS.DeleteEventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an RDS event notification subscription.
module Network.AWS.RDS.DeleteEventSubscription
  ( -- * Creating a Request
    deleteEventSubscription,
    DeleteEventSubscription,

    -- * Request Lenses
    desSubscriptionName,

    -- * Destructuring the Response
    deleteEventSubscriptionResponse,
    DeleteEventSubscriptionResponse,

    -- * Response Lenses
    delrsEventSubscription,
    delrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
  { _desSubscriptionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteEventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desSubscriptionName' - The name of the RDS event notification subscription you want to delete.
deleteEventSubscription ::
  -- | 'desSubscriptionName'
  Text ->
  DeleteEventSubscription
deleteEventSubscription pSubscriptionName_ =
  DeleteEventSubscription'
    { _desSubscriptionName =
        pSubscriptionName_
    }

-- | The name of the RDS event notification subscription you want to delete.
desSubscriptionName :: Lens' DeleteEventSubscription Text
desSubscriptionName = lens _desSubscriptionName (\s a -> s {_desSubscriptionName = a})

instance AWSRequest DeleteEventSubscription where
  type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DeleteEventSubscriptionResult"
      ( \s h x ->
          DeleteEventSubscriptionResponse'
            <$> (x .@? "EventSubscription") <*> (pure (fromEnum s))
      )

instance Hashable DeleteEventSubscription

instance NFData DeleteEventSubscription

instance ToHeaders DeleteEventSubscription where
  toHeaders = const mempty

instance ToPath DeleteEventSubscription where
  toPath = const "/"

instance ToQuery DeleteEventSubscription where
  toQuery DeleteEventSubscription' {..} =
    mconcat
      [ "Action" =: ("DeleteEventSubscription" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "SubscriptionName" =: _desSubscriptionName
      ]

-- | /See:/ 'deleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  { _delrsEventSubscription ::
      !(Maybe EventSubscription),
    _delrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsEventSubscription' - Undocumented member.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteEventSubscriptionResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteEventSubscriptionResponse
deleteEventSubscriptionResponse pResponseStatus_ =
  DeleteEventSubscriptionResponse'
    { _delrsEventSubscription =
        Nothing,
      _delrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
delrsEventSubscription :: Lens' DeleteEventSubscriptionResponse (Maybe EventSubscription)
delrsEventSubscription = lens _delrsEventSubscription (\s a -> s {_delrsEventSubscription = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteEventSubscriptionResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteEventSubscriptionResponse
