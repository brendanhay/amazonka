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
-- Module      : Network.AWS.MQ.DeleteBroker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a broker. Note: This API is asynchronous.
module Network.AWS.MQ.DeleteBroker
    (
    -- * Creating a Request
      deleteBroker
    , DeleteBroker
    -- * Request Lenses
    , dbBrokerId

    -- * Destructuring the Response
    , deleteBrokerResponse
    , DeleteBrokerResponse
    -- * Response Lenses
    , drsBrokerId
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBroker' smart constructor.
newtype DeleteBroker = DeleteBroker'
  { _dbBrokerId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBroker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbBrokerId' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
deleteBroker
    :: Text -- ^ 'dbBrokerId'
    -> DeleteBroker
deleteBroker pBrokerId_ = DeleteBroker' {_dbBrokerId = pBrokerId_}


-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
dbBrokerId :: Lens' DeleteBroker Text
dbBrokerId = lens _dbBrokerId (\ s a -> s{_dbBrokerId = a})

instance AWSRequest DeleteBroker where
        type Rs DeleteBroker = DeleteBrokerResponse
        request = delete mq
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBrokerResponse' <$>
                   (x .?> "brokerId") <*> (pure (fromEnum s)))

instance Hashable DeleteBroker where

instance NFData DeleteBroker where

instance ToHeaders DeleteBroker where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBroker where
        toPath DeleteBroker'{..}
          = mconcat ["/v1/brokers/", toBS _dbBrokerId]

instance ToQuery DeleteBroker where
        toQuery = const mempty

-- | /See:/ 'deleteBrokerResponse' smart constructor.
data DeleteBrokerResponse = DeleteBrokerResponse'
  { _drsBrokerId       :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBrokerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsBrokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteBrokerResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteBrokerResponse
deleteBrokerResponse pResponseStatus_ =
  DeleteBrokerResponse'
    {_drsBrokerId = Nothing, _drsResponseStatus = pResponseStatus_}


-- | The unique ID that Amazon MQ generates for the broker.
drsBrokerId :: Lens' DeleteBrokerResponse (Maybe Text)
drsBrokerId = lens _drsBrokerId (\ s a -> s{_drsBrokerId = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteBrokerResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteBrokerResponse where
