{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.RemovePermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes the permission of another AWS account to be able to put events to the specified event bus. Specify the account to revoke by the @StatementId@ value that you associated with the account when you granted it permission with @PutPermission@ . You can find the @StatementId@ by using 'DescribeEventBus' .
--
--
module Network.AWS.CloudWatchEvents.RemovePermission
    (
    -- * Creating a Request
      removePermission
    , RemovePermission
    -- * Request Lenses
    , rpEventBusName
    , rpStatementId

    -- * Destructuring the Response
    , removePermissionResponse
    , RemovePermissionResponse
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removePermission' smart constructor.
data RemovePermission = RemovePermission'
  { _rpEventBusName :: !(Maybe Text)
  , _rpStatementId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemovePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpEventBusName' - The name of the event bus to revoke permissions for. If you omit this, the default event bus is used.
--
-- * 'rpStatementId' - The statement ID corresponding to the account that is no longer allowed to put events to the default event bus.
removePermission
    :: Text -- ^ 'rpStatementId'
    -> RemovePermission
removePermission pStatementId_ =
  RemovePermission' {_rpEventBusName = Nothing, _rpStatementId = pStatementId_}


-- | The name of the event bus to revoke permissions for. If you omit this, the default event bus is used.
rpEventBusName :: Lens' RemovePermission (Maybe Text)
rpEventBusName = lens _rpEventBusName (\ s a -> s{_rpEventBusName = a})

-- | The statement ID corresponding to the account that is no longer allowed to put events to the default event bus.
rpStatementId :: Lens' RemovePermission Text
rpStatementId = lens _rpStatementId (\ s a -> s{_rpStatementId = a})

instance AWSRequest RemovePermission where
        type Rs RemovePermission = RemovePermissionResponse
        request = postJSON cloudWatchEvents
        response = receiveNull RemovePermissionResponse'

instance Hashable RemovePermission where

instance NFData RemovePermission where

instance ToHeaders RemovePermission where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.RemovePermission" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemovePermission where
        toJSON RemovePermission'{..}
          = object
              (catMaybes
                 [("EventBusName" .=) <$> _rpEventBusName,
                  Just ("StatementId" .= _rpStatementId)])

instance ToPath RemovePermission where
        toPath = const "/"

instance ToQuery RemovePermission where
        toQuery = const mempty

-- | /See:/ 'removePermissionResponse' smart constructor.
data RemovePermissionResponse =
  RemovePermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemovePermissionResponse' with the minimum fields required to make a request.
--
removePermissionResponse
    :: RemovePermissionResponse
removePermissionResponse = RemovePermissionResponse'


instance NFData RemovePermissionResponse where
