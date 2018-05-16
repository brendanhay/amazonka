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
-- Module      : Network.AWS.MechanicalTurk.DeleteHIT
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteHIT@ operation is used to delete HIT that is no longer needed. Only the Requester who created the HIT can delete it.
--
--
-- You can only dispose of HITs that are in the @Reviewable@ state, with all of their submitted assignments already either approved or rejected. If you call the DeleteHIT operation on a HIT that is not in the @Reviewable@ state (for example, that has not expired, or still has active assignments), or on a HIT that is Reviewable but without all of its submitted assignments already approved or rejected, the service will return an error.
--
module Network.AWS.MechanicalTurk.DeleteHIT
    (
    -- * Creating a Request
      deleteHIT
    , DeleteHIT
    -- * Request Lenses
    , dhitHITId

    -- * Destructuring the Response
    , deleteHITResponse
    , DeleteHITResponse
    -- * Response Lenses
    , dhitrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteHIT' smart constructor.
newtype DeleteHIT = DeleteHIT'
  { _dhitHITId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHIT' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhitHITId' - The ID of the HIT to be deleted.
deleteHIT
    :: Text -- ^ 'dhitHITId'
    -> DeleteHIT
deleteHIT pHITId_ = DeleteHIT' {_dhitHITId = pHITId_}


-- | The ID of the HIT to be deleted.
dhitHITId :: Lens' DeleteHIT Text
dhitHITId = lens _dhitHITId (\ s a -> s{_dhitHITId = a})

instance AWSRequest DeleteHIT where
        type Rs DeleteHIT = DeleteHITResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteHITResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteHIT where

instance NFData DeleteHIT where

instance ToHeaders DeleteHIT where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.DeleteHIT" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteHIT where
        toJSON DeleteHIT'{..}
          = object (catMaybes [Just ("HITId" .= _dhitHITId)])

instance ToPath DeleteHIT where
        toPath = const "/"

instance ToQuery DeleteHIT where
        toQuery = const mempty

-- | /See:/ 'deleteHITResponse' smart constructor.
newtype DeleteHITResponse = DeleteHITResponse'
  { _dhitrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHITResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhitrsResponseStatus' - -- | The response status code.
deleteHITResponse
    :: Int -- ^ 'dhitrsResponseStatus'
    -> DeleteHITResponse
deleteHITResponse pResponseStatus_ =
  DeleteHITResponse' {_dhitrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dhitrsResponseStatus :: Lens' DeleteHITResponse Int
dhitrsResponseStatus = lens _dhitrsResponseStatus (\ s a -> s{_dhitrsResponseStatus = a})

instance NFData DeleteHITResponse where
