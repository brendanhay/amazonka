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
-- Module      : Network.AWS.Inspector.DeleteApplication
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the application specified by the application ARN.
module Network.AWS.Inspector.DeleteApplication
    (
    -- * Creating a Request
      deleteApplication
    , DeleteApplication
    -- * Request Lenses
    , dApplicationARN

    -- * Destructuring the Response
    , deleteApplicationResponse
    , DeleteApplicationResponse
    -- * Response Lenses
    , drsMessage
    , drsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteApplication' smart constructor.
newtype DeleteApplication = DeleteApplication'
    { _dApplicationARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dApplicationARN'
deleteApplication
    :: Text -- ^ 'dApplicationARN'
    -> DeleteApplication
deleteApplication pApplicationARN_ =
    DeleteApplication'
    { _dApplicationARN = pApplicationARN_
    }

-- | The ARN specifying the application that you want to delete.
dApplicationARN :: Lens' DeleteApplication Text
dApplicationARN = lens _dApplicationARN (\ s a -> s{_dApplicationARN = a});

instance AWSRequest DeleteApplication where
        type Rs DeleteApplication = DeleteApplicationResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DeleteApplicationResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance Hashable DeleteApplication

instance NFData DeleteApplication

instance ToHeaders DeleteApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DeleteApplication" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteApplication where
        toJSON DeleteApplication'{..}
          = object
              (catMaybes
                 [Just ("applicationArn" .= _dApplicationARN)])

instance ToPath DeleteApplication where
        toPath = const "/"

instance ToQuery DeleteApplication where
        toQuery = const mempty

-- | /See:/ 'deleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
    { _drsMessage        :: !(Maybe Text)
    , _drsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsMessage'
--
-- * 'drsResponseStatus'
deleteApplicationResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteApplicationResponse
deleteApplicationResponse pResponseStatus_ =
    DeleteApplicationResponse'
    { _drsMessage = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
drsMessage :: Lens' DeleteApplicationResponse (Maybe Text)
drsMessage = lens _drsMessage (\ s a -> s{_drsMessage = a});

-- | The response status code.
drsResponseStatus :: Lens' DeleteApplicationResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});
