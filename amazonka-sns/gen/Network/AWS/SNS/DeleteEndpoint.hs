{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.DeleteEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the endpoint from Amazon SNS. This action is idempotent. For
-- more information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_DeleteEndpoint.html>
module Network.AWS.SNS.DeleteEndpoint
    (
    -- * Request
      DeleteEndpoint
    -- ** Request constructor
    , deleteEndpoint
    -- ** Request lenses
    , derqEndpointARN

    -- * Response
    , DeleteEndpointResponse
    -- ** Response constructor
    , deleteEndpointResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for DeleteEndpoint action.
--
-- /See:/ 'deleteEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derqEndpointARN'
newtype DeleteEndpoint = DeleteEndpoint'
    { _derqEndpointARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteEndpoint' smart constructor.
deleteEndpoint :: Text -> DeleteEndpoint
deleteEndpoint pEndpointARN_ =
    DeleteEndpoint'
    { _derqEndpointARN = pEndpointARN_
    }

-- | EndpointArn of endpoint to delete.
derqEndpointARN :: Lens' DeleteEndpoint Text
derqEndpointARN = lens _derqEndpointARN (\ s a -> s{_derqEndpointARN = a});

instance AWSRequest DeleteEndpoint where
        type Sv DeleteEndpoint = SNS
        type Rs DeleteEndpoint = DeleteEndpointResponse
        request = post
        response = receiveNull DeleteEndpointResponse'

instance ToHeaders DeleteEndpoint where
        toHeaders = const mempty

instance ToPath DeleteEndpoint where
        toPath = const "/"

instance ToQuery DeleteEndpoint where
        toQuery DeleteEndpoint'{..}
          = mconcat
              ["Action" =: ("DeleteEndpoint" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "EndpointArn" =: _derqEndpointARN]

-- | /See:/ 'deleteEndpointResponse' smart constructor.
data DeleteEndpointResponse =
    DeleteEndpointResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteEndpointResponse' smart constructor.
deleteEndpointResponse :: DeleteEndpointResponse
deleteEndpointResponse = DeleteEndpointResponse'
