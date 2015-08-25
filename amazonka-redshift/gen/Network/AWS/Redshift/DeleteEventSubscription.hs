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
-- Module      : Network.AWS.Redshift.DeleteEventSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Redshift event notification subscription.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteEventSubscription.html AWS API Reference> for DeleteEventSubscription.
module Network.AWS.Redshift.DeleteEventSubscription
    (
    -- * Creating a Request
      deleteEventSubscription
    , DeleteEventSubscription
    -- * Request Lenses
    , desSubscriptionName

    -- * Destructuring the Response
    , deleteEventSubscriptionResponse
    , DeleteEventSubscriptionResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
    { _desSubscriptionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteEventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desSubscriptionName'
deleteEventSubscription
    :: Text -- ^ 'desSubscriptionName'
    -> DeleteEventSubscription
deleteEventSubscription pSubscriptionName_ =
    DeleteEventSubscription'
    { _desSubscriptionName = pSubscriptionName_
    }

-- | The name of the Amazon Redshift event notification subscription to be
-- deleted.
desSubscriptionName :: Lens' DeleteEventSubscription Text
desSubscriptionName = lens _desSubscriptionName (\ s a -> s{_desSubscriptionName = a});

instance AWSRequest DeleteEventSubscription where
        type Rs DeleteEventSubscription =
             DeleteEventSubscriptionResponse
        request = postQuery redshift
        response
          = receiveNull DeleteEventSubscriptionResponse'

instance ToHeaders DeleteEventSubscription where
        toHeaders = const mempty

instance ToPath DeleteEventSubscription where
        toPath = const "/"

instance ToQuery DeleteEventSubscription where
        toQuery DeleteEventSubscription'{..}
          = mconcat
              ["Action" =:
                 ("DeleteEventSubscription" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SubscriptionName" =: _desSubscriptionName]

-- | /See:/ 'deleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse =
    DeleteEventSubscriptionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteEventSubscriptionResponse' with the minimum fields required to make a request.
--
deleteEventSubscriptionResponse
    :: DeleteEventSubscriptionResponse
deleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
