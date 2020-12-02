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
-- Module      : Network.AWS.WorkMail.UpdateResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates data for the resource. It must be preceded by a describe call in order to have the latest information. The dataset in the request should be the one expected when performing another describe call.
--
--
module Network.AWS.WorkMail.UpdateResource
    (
    -- * Creating a Request
      updateResource
    , UpdateResource
    -- * Request Lenses
    , urName
    , urBookingOptions
    , urOrganizationId
    , urResourceId

    -- * Destructuring the Response
    , updateResourceResponse
    , UpdateResourceResponse
    -- * Response Lenses
    , urrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'updateResource' smart constructor.
data UpdateResource = UpdateResource'
  { _urName           :: !(Maybe Text)
  , _urBookingOptions :: !(Maybe BookingOptions)
  , _urOrganizationId :: !Text
  , _urResourceId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urName' - The name of the resource to be updated.
--
-- * 'urBookingOptions' - The resource's booking options to be updated.
--
-- * 'urOrganizationId' - The identifier associated with the organization for which the resource is updated.
--
-- * 'urResourceId' - The identifier of the resource to be updated.
updateResource
    :: Text -- ^ 'urOrganizationId'
    -> Text -- ^ 'urResourceId'
    -> UpdateResource
updateResource pOrganizationId_ pResourceId_ =
  UpdateResource'
    { _urName = Nothing
    , _urBookingOptions = Nothing
    , _urOrganizationId = pOrganizationId_
    , _urResourceId = pResourceId_
    }


-- | The name of the resource to be updated.
urName :: Lens' UpdateResource (Maybe Text)
urName = lens _urName (\ s a -> s{_urName = a})

-- | The resource's booking options to be updated.
urBookingOptions :: Lens' UpdateResource (Maybe BookingOptions)
urBookingOptions = lens _urBookingOptions (\ s a -> s{_urBookingOptions = a})

-- | The identifier associated with the organization for which the resource is updated.
urOrganizationId :: Lens' UpdateResource Text
urOrganizationId = lens _urOrganizationId (\ s a -> s{_urOrganizationId = a})

-- | The identifier of the resource to be updated.
urResourceId :: Lens' UpdateResource Text
urResourceId = lens _urResourceId (\ s a -> s{_urResourceId = a})

instance AWSRequest UpdateResource where
        type Rs UpdateResource = UpdateResourceResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateResourceResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateResource where

instance NFData UpdateResource where

instance ToHeaders UpdateResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.UpdateResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateResource where
        toJSON UpdateResource'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _urName,
                  ("BookingOptions" .=) <$> _urBookingOptions,
                  Just ("OrganizationId" .= _urOrganizationId),
                  Just ("ResourceId" .= _urResourceId)])

instance ToPath UpdateResource where
        toPath = const "/"

instance ToQuery UpdateResource where
        toQuery = const mempty

-- | /See:/ 'updateResourceResponse' smart constructor.
newtype UpdateResourceResponse = UpdateResourceResponse'
  { _urrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsResponseStatus' - -- | The response status code.
updateResourceResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UpdateResourceResponse
updateResourceResponse pResponseStatus_ =
  UpdateResourceResponse' {_urrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
urrsResponseStatus :: Lens' UpdateResourceResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UpdateResourceResponse where
