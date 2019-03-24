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
-- Module      : Network.AWS.Connect.StopContact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends the contact initiated by the @StartOutboundVoiceContact@ operation.
--
--
-- If you are using an IAM account, it must have permission to the @connect:StopContact@ action.
--
module Network.AWS.Connect.StopContact
    (
    -- * Creating a Request
      stopContact
    , StopContact
    -- * Request Lenses
    , scContactId
    , scInstanceId

    -- * Destructuring the Response
    , stopContactResponse
    , StopContactResponse
    -- * Response Lenses
    , scrsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopContact' smart constructor.
data StopContact = StopContact'
  { _scContactId  :: !Text
  , _scInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scContactId' - The unique identifier of the contact to end.
--
-- * 'scInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
stopContact
    :: Text -- ^ 'scContactId'
    -> Text -- ^ 'scInstanceId'
    -> StopContact
stopContact pContactId_ pInstanceId_ =
  StopContact' {_scContactId = pContactId_, _scInstanceId = pInstanceId_}


-- | The unique identifier of the contact to end.
scContactId :: Lens' StopContact Text
scContactId = lens _scContactId (\ s a -> s{_scContactId = a})

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
scInstanceId :: Lens' StopContact Text
scInstanceId = lens _scInstanceId (\ s a -> s{_scInstanceId = a})

instance AWSRequest StopContact where
        type Rs StopContact = StopContactResponse
        request = postJSON connect
        response
          = receiveEmpty
              (\ s h x ->
                 StopContactResponse' <$> (pure (fromEnum s)))

instance Hashable StopContact where

instance NFData StopContact where

instance ToHeaders StopContact where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopContact where
        toJSON StopContact'{..}
          = object
              (catMaybes
                 [Just ("ContactId" .= _scContactId),
                  Just ("InstanceId" .= _scInstanceId)])

instance ToPath StopContact where
        toPath = const "/contact/stop"

instance ToQuery StopContact where
        toQuery = const mempty

-- | /See:/ 'stopContactResponse' smart constructor.
newtype StopContactResponse = StopContactResponse'
  { _scrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrsResponseStatus' - -- | The response status code.
stopContactResponse
    :: Int -- ^ 'scrsResponseStatus'
    -> StopContactResponse
stopContactResponse pResponseStatus_ =
  StopContactResponse' {_scrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
scrsResponseStatus :: Lens' StopContactResponse Int
scrsResponseStatus = lens _scrsResponseStatus (\ s a -> s{_scrsResponseStatus = a})

instance NFData StopContactResponse where
