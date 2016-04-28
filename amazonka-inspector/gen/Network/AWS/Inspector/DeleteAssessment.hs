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
-- Module      : Network.AWS.Inspector.DeleteAssessment
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment specified by the assessment ARN.
module Network.AWS.Inspector.DeleteAssessment
    (
    -- * Creating a Request
      deleteAssessment
    , DeleteAssessment
    -- * Request Lenses
    , daAssessmentARN

    -- * Destructuring the Response
    , deleteAssessmentResponse
    , DeleteAssessmentResponse
    -- * Response Lenses
    , daarsMessage
    , daarsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteAssessment' smart constructor.
newtype DeleteAssessment = DeleteAssessment'
    { _daAssessmentARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteAssessment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAssessmentARN'
deleteAssessment
    :: Text -- ^ 'daAssessmentARN'
    -> DeleteAssessment
deleteAssessment pAssessmentARN_ =
    DeleteAssessment'
    { _daAssessmentARN = pAssessmentARN_
    }

-- | The ARN specifying the assessment that you want to delete.
daAssessmentARN :: Lens' DeleteAssessment Text
daAssessmentARN = lens _daAssessmentARN (\ s a -> s{_daAssessmentARN = a});

instance AWSRequest DeleteAssessment where
        type Rs DeleteAssessment = DeleteAssessmentResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DeleteAssessmentResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance Hashable DeleteAssessment

instance NFData DeleteAssessment

instance ToHeaders DeleteAssessment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DeleteAssessment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAssessment where
        toJSON DeleteAssessment'{..}
          = object
              (catMaybes
                 [Just ("assessmentArn" .= _daAssessmentARN)])

instance ToPath DeleteAssessment where
        toPath = const "/"

instance ToQuery DeleteAssessment where
        toQuery = const mempty

-- | /See:/ 'deleteAssessmentResponse' smart constructor.
data DeleteAssessmentResponse = DeleteAssessmentResponse'
    { _daarsMessage        :: !(Maybe Text)
    , _daarsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteAssessmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsMessage'
--
-- * 'daarsResponseStatus'
deleteAssessmentResponse
    :: Int -- ^ 'daarsResponseStatus'
    -> DeleteAssessmentResponse
deleteAssessmentResponse pResponseStatus_ =
    DeleteAssessmentResponse'
    { _daarsMessage = Nothing
    , _daarsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
daarsMessage :: Lens' DeleteAssessmentResponse (Maybe Text)
daarsMessage = lens _daarsMessage (\ s a -> s{_daarsMessage = a});

-- | The response status code.
daarsResponseStatus :: Lens' DeleteAssessmentResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\ s a -> s{_daarsResponseStatus = a});
