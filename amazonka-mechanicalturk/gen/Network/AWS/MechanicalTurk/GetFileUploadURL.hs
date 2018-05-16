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
-- Module      : Network.AWS.MechanicalTurk.GetFileUploadURL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetFileUploadURL@ operation generates and returns a temporary URL. You use the temporary URL to retrieve a file uploaded by a Worker as an answer to a FileUploadAnswer question for a HIT. The temporary URL is generated the instant the GetFileUploadURL operation is called, and is valid for 60 seconds. You can get a temporary file upload URL any time until the HIT is disposed. After the HIT is disposed, any uploaded files are deleted, and cannot be retrieved. Pending Deprecation on December 12, 2017. The Answer Specification structure will no longer support the @FileUploadAnswer@ element to be used for the QuestionForm data structure. Instead, we recommend that Requesters who want to create HITs asking Workers to upload files to use Amazon S3.
--
--
module Network.AWS.MechanicalTurk.GetFileUploadURL
    (
    -- * Creating a Request
      getFileUploadURL
    , GetFileUploadURL
    -- * Request Lenses
    , gfuuAssignmentId
    , gfuuQuestionIdentifier

    -- * Destructuring the Response
    , getFileUploadURLResponse
    , GetFileUploadURLResponse
    -- * Response Lenses
    , gfuursFileUploadURL
    , gfuursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFileUploadURL' smart constructor.
data GetFileUploadURL = GetFileUploadURL'
  { _gfuuAssignmentId       :: !Text
  , _gfuuQuestionIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFileUploadURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfuuAssignmentId' - The ID of the assignment that contains the question with a FileUploadAnswer.
--
-- * 'gfuuQuestionIdentifier' - The identifier of the question with a FileUploadAnswer, as specified in the QuestionForm of the HIT.
getFileUploadURL
    :: Text -- ^ 'gfuuAssignmentId'
    -> Text -- ^ 'gfuuQuestionIdentifier'
    -> GetFileUploadURL
getFileUploadURL pAssignmentId_ pQuestionIdentifier_ =
  GetFileUploadURL'
    { _gfuuAssignmentId = pAssignmentId_
    , _gfuuQuestionIdentifier = pQuestionIdentifier_
    }


-- | The ID of the assignment that contains the question with a FileUploadAnswer.
gfuuAssignmentId :: Lens' GetFileUploadURL Text
gfuuAssignmentId = lens _gfuuAssignmentId (\ s a -> s{_gfuuAssignmentId = a})

-- | The identifier of the question with a FileUploadAnswer, as specified in the QuestionForm of the HIT.
gfuuQuestionIdentifier :: Lens' GetFileUploadURL Text
gfuuQuestionIdentifier = lens _gfuuQuestionIdentifier (\ s a -> s{_gfuuQuestionIdentifier = a})

instance AWSRequest GetFileUploadURL where
        type Rs GetFileUploadURL = GetFileUploadURLResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 GetFileUploadURLResponse' <$>
                   (x .?> "FileUploadURL") <*> (pure (fromEnum s)))

instance Hashable GetFileUploadURL where

instance NFData GetFileUploadURL where

instance ToHeaders GetFileUploadURL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.GetFileUploadURL" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetFileUploadURL where
        toJSON GetFileUploadURL'{..}
          = object
              (catMaybes
                 [Just ("AssignmentId" .= _gfuuAssignmentId),
                  Just
                    ("QuestionIdentifier" .= _gfuuQuestionIdentifier)])

instance ToPath GetFileUploadURL where
        toPath = const "/"

instance ToQuery GetFileUploadURL where
        toQuery = const mempty

-- | /See:/ 'getFileUploadURLResponse' smart constructor.
data GetFileUploadURLResponse = GetFileUploadURLResponse'
  { _gfuursFileUploadURL  :: !(Maybe Text)
  , _gfuursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFileUploadURLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfuursFileUploadURL' - A temporary URL for the file that the Worker uploaded for the answer.
--
-- * 'gfuursResponseStatus' - -- | The response status code.
getFileUploadURLResponse
    :: Int -- ^ 'gfuursResponseStatus'
    -> GetFileUploadURLResponse
getFileUploadURLResponse pResponseStatus_ =
  GetFileUploadURLResponse'
    {_gfuursFileUploadURL = Nothing, _gfuursResponseStatus = pResponseStatus_}


-- | A temporary URL for the file that the Worker uploaded for the answer.
gfuursFileUploadURL :: Lens' GetFileUploadURLResponse (Maybe Text)
gfuursFileUploadURL = lens _gfuursFileUploadURL (\ s a -> s{_gfuursFileUploadURL = a})

-- | -- | The response status code.
gfuursResponseStatus :: Lens' GetFileUploadURLResponse Int
gfuursResponseStatus = lens _gfuursResponseStatus (\ s a -> s{_gfuursResponseStatus = a})

instance NFData GetFileUploadURLResponse where
