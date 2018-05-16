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
-- Module      : Network.AWS.CodePipeline.ListPipelines
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all of the pipelines associated with your account.
--
--
module Network.AWS.CodePipeline.ListPipelines
    (
    -- * Creating a Request
      listPipelines
    , ListPipelines
    -- * Request Lenses
    , lpNextToken

    -- * Destructuring the Response
    , listPipelinesResponse
    , ListPipelinesResponse
    -- * Response Lenses
    , lprsPipelines
    , lprsNextToken
    , lprsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListPipelines action.
--
--
--
-- /See:/ 'listPipelines' smart constructor.
newtype ListPipelines = ListPipelines'
  { _lpNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelines' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpNextToken' - An identifier that was returned from the previous list pipelines call, which can be used to return the next set of pipelines in the list.
listPipelines
    :: ListPipelines
listPipelines = ListPipelines' {_lpNextToken = Nothing}


-- | An identifier that was returned from the previous list pipelines call, which can be used to return the next set of pipelines in the list.
lpNextToken :: Lens' ListPipelines (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a})

instance AWSRequest ListPipelines where
        type Rs ListPipelines = ListPipelinesResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' <$>
                   (x .?> "pipelines" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListPipelines where

instance NFData ListPipelines where

instance ToHeaders ListPipelines where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.ListPipelines" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPipelines where
        toJSON ListPipelines'{..}
          = object
              (catMaybes [("nextToken" .=) <$> _lpNextToken])

instance ToPath ListPipelines where
        toPath = const "/"

instance ToQuery ListPipelines where
        toQuery = const mempty

-- | Represents the output of a ListPipelines action.
--
--
--
-- /See:/ 'listPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { _lprsPipelines      :: !(Maybe [PipelineSummary])
  , _lprsNextToken      :: !(Maybe Text)
  , _lprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelinesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsPipelines' - The list of pipelines.
--
-- * 'lprsNextToken' - If the amount of returned information is significantly large, an identifier is also returned which can be used in a subsequent list pipelines call to return the next set of pipelines in the list.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPipelinesResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPipelinesResponse
listPipelinesResponse pResponseStatus_ =
  ListPipelinesResponse'
    { _lprsPipelines = Nothing
    , _lprsNextToken = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | The list of pipelines.
lprsPipelines :: Lens' ListPipelinesResponse [PipelineSummary]
lprsPipelines = lens _lprsPipelines (\ s a -> s{_lprsPipelines = a}) . _Default . _Coerce

-- | If the amount of returned information is significantly large, an identifier is also returned which can be used in a subsequent list pipelines call to return the next set of pipelines in the list.
lprsNextToken :: Lens' ListPipelinesResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a})

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPipelinesResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListPipelinesResponse where
