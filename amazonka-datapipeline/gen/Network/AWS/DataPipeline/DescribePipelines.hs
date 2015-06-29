{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DataPipeline.DescribePipelines
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves metadata about one or more pipelines. The information
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
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_DescribePipelines.html>
module Network.AWS.DataPipeline.DescribePipelines
    (
    -- * Request
      DescribePipelines
    -- ** Request constructor
    , describePipelines
    -- ** Request lenses
    , dpPipelineIds

    -- * Response
    , DescribePipelinesResponse
    -- ** Response constructor
    , describePipelinesResponse
    -- ** Response lenses
    , dprStatus
    , dprPipelineDescriptionList
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribePipelines.
--
-- /See:/ 'describePipelines' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpPipelineIds'
newtype DescribePipelines = DescribePipelines'
    { _dpPipelineIds :: [Text]
    } deriving (Eq,Read,Show)

-- | 'DescribePipelines' smart constructor.
describePipelines :: DescribePipelines
describePipelines =
    DescribePipelines'
    { _dpPipelineIds = mempty
    }

-- | The IDs of the pipelines to describe. You can pass as many as 25
-- identifiers in a single call. To obtain pipeline IDs, call
-- ListPipelines.
dpPipelineIds :: Lens' DescribePipelines [Text]
dpPipelineIds = lens _dpPipelineIds (\ s a -> s{_dpPipelineIds = a});

instance AWSRequest DescribePipelines where
        type Sv DescribePipelines = DataPipeline
        type Rs DescribePipelines = DescribePipelinesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribePipelinesResponse' <$>
                   (pure s) <*>
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
          = object ["pipelineIds" .= _dpPipelineIds]

instance ToPath DescribePipelines where
        toPath = const "/"

instance ToQuery DescribePipelines where
        toQuery = const mempty

-- | Contains the output of DescribePipelines.
--
-- /See:/ 'describePipelinesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dprStatus'
--
-- * 'dprPipelineDescriptionList'
data DescribePipelinesResponse = DescribePipelinesResponse'
    { _dprStatus                  :: !Status
    , _dprPipelineDescriptionList :: ![PipelineDescription]
    } deriving (Eq,Show)

-- | 'DescribePipelinesResponse' smart constructor.
describePipelinesResponse :: Status -> DescribePipelinesResponse
describePipelinesResponse pStatus =
    DescribePipelinesResponse'
    { _dprStatus = pStatus
    , _dprPipelineDescriptionList = mempty
    }

-- | FIXME: Undocumented member.
dprStatus :: Lens' DescribePipelinesResponse Status
dprStatus = lens _dprStatus (\ s a -> s{_dprStatus = a});

-- | An array of descriptions for the specified pipelines.
dprPipelineDescriptionList :: Lens' DescribePipelinesResponse [PipelineDescription]
dprPipelineDescriptionList = lens _dprPipelineDescriptionList (\ s a -> s{_dprPipelineDescriptionList = a});
