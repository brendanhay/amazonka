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
-- Module      : Network.AWS.CloudWatchEvents.DeletePartnerEventSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation is used by SaaS partners to delete a partner event source. AWS customers don't use this operation.
--
--
-- When you delete an event source, the status of the corresponding partner event bus in the AWS customer account becomes @DELETED@ .
--
module Network.AWS.CloudWatchEvents.DeletePartnerEventSource
    (
    -- * Creating a Request
      deletePartnerEventSource
    , DeletePartnerEventSource
    -- * Request Lenses
    , dpesName
    , dpesAccount

    -- * Destructuring the Response
    , deletePartnerEventSourceResponse
    , DeletePartnerEventSourceResponse
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePartnerEventSource' smart constructor.
data DeletePartnerEventSource = DeletePartnerEventSource'
  { _dpesName    :: !Text
  , _dpesAccount :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePartnerEventSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpesName' - The name of the event source to delete.
--
-- * 'dpesAccount' - The AWS account ID of the AWS customer that the event source was created for.
deletePartnerEventSource
    :: Text -- ^ 'dpesName'
    -> Text -- ^ 'dpesAccount'
    -> DeletePartnerEventSource
deletePartnerEventSource pName_ pAccount_ =
  DeletePartnerEventSource' {_dpesName = pName_, _dpesAccount = pAccount_}


-- | The name of the event source to delete.
dpesName :: Lens' DeletePartnerEventSource Text
dpesName = lens _dpesName (\ s a -> s{_dpesName = a})

-- | The AWS account ID of the AWS customer that the event source was created for.
dpesAccount :: Lens' DeletePartnerEventSource Text
dpesAccount = lens _dpesAccount (\ s a -> s{_dpesAccount = a})

instance AWSRequest DeletePartnerEventSource where
        type Rs DeletePartnerEventSource =
             DeletePartnerEventSourceResponse
        request = postJSON cloudWatchEvents
        response
          = receiveNull DeletePartnerEventSourceResponse'

instance Hashable DeletePartnerEventSource where

instance NFData DeletePartnerEventSource where

instance ToHeaders DeletePartnerEventSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.DeletePartnerEventSource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePartnerEventSource where
        toJSON DeletePartnerEventSource'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _dpesName),
                  Just ("Account" .= _dpesAccount)])

instance ToPath DeletePartnerEventSource where
        toPath = const "/"

instance ToQuery DeletePartnerEventSource where
        toQuery = const mempty

-- | /See:/ 'deletePartnerEventSourceResponse' smart constructor.
data DeletePartnerEventSourceResponse =
  DeletePartnerEventSourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePartnerEventSourceResponse' with the minimum fields required to make a request.
--
deletePartnerEventSourceResponse
    :: DeletePartnerEventSourceResponse
deletePartnerEventSourceResponse = DeletePartnerEventSourceResponse'


instance NFData DeletePartnerEventSourceResponse
         where
