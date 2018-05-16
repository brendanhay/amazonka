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
-- Module      : Network.AWS.CodePipeline.PutApprovalResult
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the response to a manual approval request to AWS CodePipeline. Valid responses include Approved and Rejected.
--
--
module Network.AWS.CodePipeline.PutApprovalResult
    (
    -- * Creating a Request
      putApprovalResult
    , PutApprovalResult
    -- * Request Lenses
    , parPipelineName
    , parStageName
    , parActionName
    , parResult
    , parToken

    -- * Destructuring the Response
    , putApprovalResultResponse
    , PutApprovalResultResponse
    -- * Response Lenses
    , parrsApprovedAt
    , parrsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a PutApprovalResult action.
--
--
--
-- /See:/ 'putApprovalResult' smart constructor.
data PutApprovalResult = PutApprovalResult'
  { _parPipelineName :: !Text
  , _parStageName    :: !Text
  , _parActionName   :: !Text
  , _parResult       :: !ApprovalResult
  , _parToken        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutApprovalResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parPipelineName' - The name of the pipeline that contains the action.
--
-- * 'parStageName' - The name of the stage that contains the action.
--
-- * 'parActionName' - The name of the action for which approval is requested.
--
-- * 'parResult' - Represents information about the result of the approval request.
--
-- * 'parToken' - The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action and is used to validate that the approval request corresponding to this token is still valid.
putApprovalResult
    :: Text -- ^ 'parPipelineName'
    -> Text -- ^ 'parStageName'
    -> Text -- ^ 'parActionName'
    -> ApprovalResult -- ^ 'parResult'
    -> Text -- ^ 'parToken'
    -> PutApprovalResult
putApprovalResult pPipelineName_ pStageName_ pActionName_ pResult_ pToken_ =
  PutApprovalResult'
    { _parPipelineName = pPipelineName_
    , _parStageName = pStageName_
    , _parActionName = pActionName_
    , _parResult = pResult_
    , _parToken = pToken_
    }


-- | The name of the pipeline that contains the action.
parPipelineName :: Lens' PutApprovalResult Text
parPipelineName = lens _parPipelineName (\ s a -> s{_parPipelineName = a})

-- | The name of the stage that contains the action.
parStageName :: Lens' PutApprovalResult Text
parStageName = lens _parStageName (\ s a -> s{_parStageName = a})

-- | The name of the action for which approval is requested.
parActionName :: Lens' PutApprovalResult Text
parActionName = lens _parActionName (\ s a -> s{_parActionName = a})

-- | Represents information about the result of the approval request.
parResult :: Lens' PutApprovalResult ApprovalResult
parResult = lens _parResult (\ s a -> s{_parResult = a})

-- | The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the 'GetPipelineState' action and is used to validate that the approval request corresponding to this token is still valid.
parToken :: Lens' PutApprovalResult Text
parToken = lens _parToken (\ s a -> s{_parToken = a})

instance AWSRequest PutApprovalResult where
        type Rs PutApprovalResult = PutApprovalResultResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 PutApprovalResultResponse' <$>
                   (x .?> "approvedAt") <*> (pure (fromEnum s)))

instance Hashable PutApprovalResult where

instance NFData PutApprovalResult where

instance ToHeaders PutApprovalResult where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.PutApprovalResult" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutApprovalResult where
        toJSON PutApprovalResult'{..}
          = object
              (catMaybes
                 [Just ("pipelineName" .= _parPipelineName),
                  Just ("stageName" .= _parStageName),
                  Just ("actionName" .= _parActionName),
                  Just ("result" .= _parResult),
                  Just ("token" .= _parToken)])

instance ToPath PutApprovalResult where
        toPath = const "/"

instance ToQuery PutApprovalResult where
        toQuery = const mempty

-- | Represents the output of a PutApprovalResult action.
--
--
--
-- /See:/ 'putApprovalResultResponse' smart constructor.
data PutApprovalResultResponse = PutApprovalResultResponse'
  { _parrsApprovedAt     :: !(Maybe POSIX)
  , _parrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutApprovalResultResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parrsApprovedAt' - The timestamp showing when the approval or rejection was submitted.
--
-- * 'parrsResponseStatus' - -- | The response status code.
putApprovalResultResponse
    :: Int -- ^ 'parrsResponseStatus'
    -> PutApprovalResultResponse
putApprovalResultResponse pResponseStatus_ =
  PutApprovalResultResponse'
    {_parrsApprovedAt = Nothing, _parrsResponseStatus = pResponseStatus_}


-- | The timestamp showing when the approval or rejection was submitted.
parrsApprovedAt :: Lens' PutApprovalResultResponse (Maybe UTCTime)
parrsApprovedAt = lens _parrsApprovedAt (\ s a -> s{_parrsApprovedAt = a}) . mapping _Time

-- | -- | The response status code.
parrsResponseStatus :: Lens' PutApprovalResultResponse Int
parrsResponseStatus = lens _parrsResponseStatus (\ s a -> s{_parrsResponseStatus = a})

instance NFData PutApprovalResultResponse where
