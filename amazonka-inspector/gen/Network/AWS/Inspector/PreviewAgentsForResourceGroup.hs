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
-- Module      : Network.AWS.Inspector.PreviewAgentsForResourceGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Previews the agents installed on the EC2 instances that are included in
-- the application created with the specified resource group.
module Network.AWS.Inspector.PreviewAgentsForResourceGroup
    (
    -- * Creating a Request
      previewAgentsForResourceGroup
    , PreviewAgentsForResourceGroup
    -- * Request Lenses
    , pafrgNextToken
    , pafrgMaxResults
    , pafrgResourceGroupARN

    -- * Destructuring the Response
    , previewAgentsForResourceGroupResponse
    , PreviewAgentsForResourceGroupResponse
    -- * Response Lenses
    , pafrgrsAgentPreviewList
    , pafrgrsNextToken
    , pafrgrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'previewAgentsForResourceGroup' smart constructor.
data PreviewAgentsForResourceGroup = PreviewAgentsForResourceGroup'
    { _pafrgNextToken        :: !(Maybe Text)
    , _pafrgMaxResults       :: !(Maybe Int)
    , _pafrgResourceGroupARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PreviewAgentsForResourceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pafrgNextToken'
--
-- * 'pafrgMaxResults'
--
-- * 'pafrgResourceGroupARN'
previewAgentsForResourceGroup
    :: Text -- ^ 'pafrgResourceGroupARN'
    -> PreviewAgentsForResourceGroup
previewAgentsForResourceGroup pResourceGroupARN_ =
    PreviewAgentsForResourceGroup'
    { _pafrgNextToken = Nothing
    , _pafrgMaxResults = Nothing
    , _pafrgResourceGroupARN = pResourceGroupARN_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to \'null\' on your first call to the
-- __PreviewAgentsForResourceGroup__ action. Subsequent calls to the action
-- fill __nextToken__ in the request with the value of __NextToken__ from
-- previous response to continue listing data.
pafrgNextToken :: Lens' PreviewAgentsForResourceGroup (Maybe Text)
pafrgNextToken = lens _pafrgNextToken (\ s a -> s{_pafrgNextToken = a});

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
pafrgMaxResults :: Lens' PreviewAgentsForResourceGroup (Maybe Int)
pafrgMaxResults = lens _pafrgMaxResults (\ s a -> s{_pafrgMaxResults = a});

-- | The ARN of the resource group that is used to create an application.
pafrgResourceGroupARN :: Lens' PreviewAgentsForResourceGroup Text
pafrgResourceGroupARN = lens _pafrgResourceGroupARN (\ s a -> s{_pafrgResourceGroupARN = a});

instance AWSRequest PreviewAgentsForResourceGroup
         where
        type Rs PreviewAgentsForResourceGroup =
             PreviewAgentsForResourceGroupResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 PreviewAgentsForResourceGroupResponse' <$>
                   (x .?> "agentPreviewList" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable PreviewAgentsForResourceGroup

instance ToHeaders PreviewAgentsForResourceGroup
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.PreviewAgentsForResourceGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PreviewAgentsForResourceGroup where
        toJSON PreviewAgentsForResourceGroup'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _pafrgNextToken,
                  ("maxResults" .=) <$> _pafrgMaxResults,
                  Just ("resourceGroupArn" .= _pafrgResourceGroupARN)])

instance ToPath PreviewAgentsForResourceGroup where
        toPath = const "/"

instance ToQuery PreviewAgentsForResourceGroup where
        toQuery = const mempty

-- | /See:/ 'previewAgentsForResourceGroupResponse' smart constructor.
data PreviewAgentsForResourceGroupResponse = PreviewAgentsForResourceGroupResponse'
    { _pafrgrsAgentPreviewList :: !(Maybe [AgentPreview])
    , _pafrgrsNextToken        :: !(Maybe Text)
    , _pafrgrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PreviewAgentsForResourceGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pafrgrsAgentPreviewList'
--
-- * 'pafrgrsNextToken'
--
-- * 'pafrgrsResponseStatus'
previewAgentsForResourceGroupResponse
    :: Int -- ^ 'pafrgrsResponseStatus'
    -> PreviewAgentsForResourceGroupResponse
previewAgentsForResourceGroupResponse pResponseStatus_ =
    PreviewAgentsForResourceGroupResponse'
    { _pafrgrsAgentPreviewList = Nothing
    , _pafrgrsNextToken = Nothing
    , _pafrgrsResponseStatus = pResponseStatus_
    }

-- | The resulting list of agents.
pafrgrsAgentPreviewList :: Lens' PreviewAgentsForResourceGroupResponse [AgentPreview]
pafrgrsAgentPreviewList = lens _pafrgrsAgentPreviewList (\ s a -> s{_pafrgrsAgentPreviewList = a}) . _Default . _Coerce;

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to \'null\'.
pafrgrsNextToken :: Lens' PreviewAgentsForResourceGroupResponse (Maybe Text)
pafrgrsNextToken = lens _pafrgrsNextToken (\ s a -> s{_pafrgrsNextToken = a});

-- | The response status code.
pafrgrsResponseStatus :: Lens' PreviewAgentsForResourceGroupResponse Int
pafrgrsResponseStatus = lens _pafrgrsResponseStatus (\ s a -> s{_pafrgrsResponseStatus = a});
