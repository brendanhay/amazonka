{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListPipelines
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all of the pipelines associated with your account.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_ListPipelines.html AWS API Reference> for ListPipelines.
module Network.AWS.CodePipeline.ListPipelines
    (
    -- * Creating a Request
      ListPipelines
    , listPipelines
    -- * Request Lenses
    , lpNextToken

    -- * Destructuring the Response
    , ListPipelinesResponse
    , listPipelinesResponse
    -- * Response Lenses
    , lprsPipelines
    , lprsNextToken
    , lprsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list pipelines action.
--
-- /See:/ 'listPipelines' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpNextToken'
newtype ListPipelines = ListPipelines'
    { _lpNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPipelines' smart constructor.
listPipelines :: ListPipelines
listPipelines =
    ListPipelines'
    { _lpNextToken = Nothing
    }

-- | An identifier that was returned from the previous list pipelines call,
-- which can be used to return the next set of pipelines in the list.
lpNextToken :: Lens' ListPipelines (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a});

instance AWSRequest ListPipelines where
        type Sv ListPipelines = CodePipeline
        type Rs ListPipelines = ListPipelinesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' <$>
                   (x .?> "pipelines" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

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
          = object ["nextToken" .= _lpNextToken]

instance ToPath ListPipelines where
        toPath = const "/"

instance ToQuery ListPipelines where
        toQuery = const mempty

-- | Represents the output of a list pipelines action.
--
-- /See:/ 'listPipelinesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprsPipelines'
--
-- * 'lprsNextToken'
--
-- * 'lprsStatus'
data ListPipelinesResponse = ListPipelinesResponse'
    { _lprsPipelines :: !(Maybe [PipelineSummary])
    , _lprsNextToken :: !(Maybe Text)
    , _lprsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPipelinesResponse' smart constructor.
listPipelinesResponse :: Int -> ListPipelinesResponse
listPipelinesResponse pStatus_ =
    ListPipelinesResponse'
    { _lprsPipelines = Nothing
    , _lprsNextToken = Nothing
    , _lprsStatus = pStatus_
    }

-- | The list of pipelines.
lprsPipelines :: Lens' ListPipelinesResponse [PipelineSummary]
lprsPipelines = lens _lprsPipelines (\ s a -> s{_lprsPipelines = a}) . _Default . _Coerce;

-- | If the amount of returned information is significantly large, an
-- identifier is also returned which can be used in a subsequent list
-- pipelines call to return the next set of pipelines in the list.
lprsNextToken :: Lens' ListPipelinesResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a});

-- | Undocumented member.
lprsStatus :: Lens' ListPipelinesResponse Int
lprsStatus = lens _lprsStatus (\ s a -> s{_lprsStatus = a});
