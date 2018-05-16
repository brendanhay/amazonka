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
-- Module      : Network.AWS.Inspector.PreviewAgents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Previews the agents installed on the EC2 instances that are part of the specified assessment target.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Inspector.PreviewAgents
    (
    -- * Creating a Request
      previewAgents
    , PreviewAgents
    -- * Request Lenses
    , paNextToken
    , paMaxResults
    , paPreviewAgentsARN

    -- * Destructuring the Response
    , previewAgentsResponse
    , PreviewAgentsResponse
    -- * Response Lenses
    , parsNextToken
    , parsResponseStatus
    , parsAgentPreviews
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'previewAgents' smart constructor.
data PreviewAgents = PreviewAgents'
  { _paNextToken        :: !(Maybe Text)
  , _paMaxResults       :: !(Maybe Int)
  , _paPreviewAgentsARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PreviewAgents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __PreviewAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- * 'paMaxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
--
-- * 'paPreviewAgentsARN' - The ARN of the assessment target whose agents you want to preview.
previewAgents
    :: Text -- ^ 'paPreviewAgentsARN'
    -> PreviewAgents
previewAgents pPreviewAgentsARN_ =
  PreviewAgents'
    { _paNextToken = Nothing
    , _paMaxResults = Nothing
    , _paPreviewAgentsARN = pPreviewAgentsARN_
    }


-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __PreviewAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
paNextToken :: Lens' PreviewAgents (Maybe Text)
paNextToken = lens _paNextToken (\ s a -> s{_paNextToken = a})

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
paMaxResults :: Lens' PreviewAgents (Maybe Int)
paMaxResults = lens _paMaxResults (\ s a -> s{_paMaxResults = a})

-- | The ARN of the assessment target whose agents you want to preview.
paPreviewAgentsARN :: Lens' PreviewAgents Text
paPreviewAgentsARN = lens _paPreviewAgentsARN (\ s a -> s{_paPreviewAgentsARN = a})

instance AWSPager PreviewAgents where
        page rq rs
          | stop (rs ^. parsNextToken) = Nothing
          | stop (rs ^. parsAgentPreviews) = Nothing
          | otherwise =
            Just $ rq & paNextToken .~ rs ^. parsNextToken

instance AWSRequest PreviewAgents where
        type Rs PreviewAgents = PreviewAgentsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 PreviewAgentsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "agentPreviews" .!@ mempty))

instance Hashable PreviewAgents where

instance NFData PreviewAgents where

instance ToHeaders PreviewAgents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.PreviewAgents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PreviewAgents where
        toJSON PreviewAgents'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _paNextToken,
                  ("maxResults" .=) <$> _paMaxResults,
                  Just ("previewAgentsArn" .= _paPreviewAgentsARN)])

instance ToPath PreviewAgents where
        toPath = const "/"

instance ToQuery PreviewAgents where
        toQuery = const mempty

-- | /See:/ 'previewAgentsResponse' smart constructor.
data PreviewAgentsResponse = PreviewAgentsResponse'
  { _parsNextToken      :: !(Maybe Text)
  , _parsResponseStatus :: !Int
  , _parsAgentPreviews  :: ![AgentPreview]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PreviewAgentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parsNextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- * 'parsResponseStatus' - -- | The response status code.
--
-- * 'parsAgentPreviews' - The resulting list of agents.
previewAgentsResponse
    :: Int -- ^ 'parsResponseStatus'
    -> PreviewAgentsResponse
previewAgentsResponse pResponseStatus_ =
  PreviewAgentsResponse'
    { _parsNextToken = Nothing
    , _parsResponseStatus = pResponseStatus_
    , _parsAgentPreviews = mempty
    }


-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
parsNextToken :: Lens' PreviewAgentsResponse (Maybe Text)
parsNextToken = lens _parsNextToken (\ s a -> s{_parsNextToken = a})

-- | -- | The response status code.
parsResponseStatus :: Lens' PreviewAgentsResponse Int
parsResponseStatus = lens _parsResponseStatus (\ s a -> s{_parsResponseStatus = a})

-- | The resulting list of agents.
parsAgentPreviews :: Lens' PreviewAgentsResponse [AgentPreview]
parsAgentPreviews = lens _parsAgentPreviews (\ s a -> s{_parsAgentPreviews = a}) . _Coerce

instance NFData PreviewAgentsResponse where
