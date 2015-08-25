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
-- Module      : Network.AWS.DataPipeline.DescribePipelines
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about one or more pipelines. The information
-- retrieved includes the name of the pipeline, the pipeline identifier,
-- its current state, and the user account that owns the pipeline. Using
-- account credentials, you can retrieve metadata about pipelines that you
-- or your IAM users have created. If you are using an IAM user account,
-- you can retrieve metadata about only those pipelines for which you have
-- read permissions.
--
-- To retrieve the full pipeline definition instead of metadata about the
-- pipeline, call GetPipelineDefinition.
--
-- /See:/ <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_DescribePipelines.html AWS API Reference> for DescribePipelines.
module Network.AWS.DataPipeline.DescribePipelines
    (
    -- * Creating a Request
      describePipelines
    , DescribePipelines
    -- * Request Lenses
    , dpPipelineIds

    -- * Destructuring the Response
    , describePipelinesResponse
    , DescribePipelinesResponse
    -- * Response Lenses
    , dprsStatus
    , dprsPipelineDescriptionList
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.DataPipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribePipelines.
--
-- /See:/ 'describePipelines' smart constructor.
newtype DescribePipelines = DescribePipelines'
    { _dpPipelineIds :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribePipelines' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpPipelineIds'
describePipelines
    :: DescribePipelines
describePipelines =
    DescribePipelines'
    { _dpPipelineIds = mempty
    }

-- | The IDs of the pipelines to describe. You can pass as many as 25
-- identifiers in a single call. To obtain pipeline IDs, call
-- ListPipelines.
dpPipelineIds :: Lens' DescribePipelines [Text]
dpPipelineIds = lens _dpPipelineIds (\ s a -> s{_dpPipelineIds = a}) . _Coerce;

instance AWSRequest DescribePipelines where
        type Rs DescribePipelines = DescribePipelinesResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 DescribePipelinesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "pipelineDescriptionList" .!@ mempty))

instance ToHeaders DescribePipelines where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.DescribePipelines" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePipelines where
        toJSON DescribePipelines'{..}
          = object
              (catMaybes [Just ("pipelineIds" .= _dpPipelineIds)])

instance ToPath DescribePipelines where
        toPath = const "/"

instance ToQuery DescribePipelines where
        toQuery = const mempty

-- | Contains the output of DescribePipelines.
--
-- /See:/ 'describePipelinesResponse' smart constructor.
data DescribePipelinesResponse = DescribePipelinesResponse'
    { _dprsStatus                  :: !Int
    , _dprsPipelineDescriptionList :: ![PipelineDescription]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribePipelinesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsStatus'
--
-- * 'dprsPipelineDescriptionList'
describePipelinesResponse
    :: Int -- ^ 'dprsStatus'
    -> DescribePipelinesResponse
describePipelinesResponse pStatus_ =
    DescribePipelinesResponse'
    { _dprsStatus = pStatus_
    , _dprsPipelineDescriptionList = mempty
    }

-- | The response status code.
dprsStatus :: Lens' DescribePipelinesResponse Int
dprsStatus = lens _dprsStatus (\ s a -> s{_dprsStatus = a});

-- | An array of descriptions for the specified pipelines.
dprsPipelineDescriptionList :: Lens' DescribePipelinesResponse [PipelineDescription]
dprsPipelineDescriptionList = lens _dprsPipelineDescriptionList (\ s a -> s{_dprsPipelineDescriptionList = a}) . _Coerce;
