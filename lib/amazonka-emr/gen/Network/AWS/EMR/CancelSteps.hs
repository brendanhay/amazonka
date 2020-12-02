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
-- Module      : Network.AWS.EMR.CancelSteps
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending step or steps in a running cluster. Available only in Amazon EMR versions 4.8.0 and later, excluding version 5.0.0. A maximum of 256 steps are allowed in each CancelSteps request. CancelSteps is idempotent but asynchronous; it does not guarantee a step will be canceled, even if the request is successfully submitted. You can only cancel steps that are in a @PENDING@ state.
--
--
module Network.AWS.EMR.CancelSteps
    (
    -- * Creating a Request
      cancelSteps
    , CancelSteps
    -- * Request Lenses
    , csStepIds
    , csClusterId

    -- * Destructuring the Response
    , cancelStepsResponse
    , CancelStepsResponse
    -- * Response Lenses
    , csrsCancelStepsInfoList
    , csrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input argument to the 'CancelSteps' operation.
--
--
--
-- /See:/ 'cancelSteps' smart constructor.
data CancelSteps = CancelSteps'
  { _csStepIds   :: !(Maybe [Text])
  , _csClusterId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelSteps' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csStepIds' - The list of @StepIDs@ to cancel. Use 'ListSteps' to get steps and their states for the specified cluster.
--
-- * 'csClusterId' - The @ClusterID@ for which specified steps will be canceled. Use 'RunJobFlow' and 'ListClusters' to get ClusterIDs.
cancelSteps
    :: CancelSteps
cancelSteps = CancelSteps' {_csStepIds = Nothing, _csClusterId = Nothing}


-- | The list of @StepIDs@ to cancel. Use 'ListSteps' to get steps and their states for the specified cluster.
csStepIds :: Lens' CancelSteps [Text]
csStepIds = lens _csStepIds (\ s a -> s{_csStepIds = a}) . _Default . _Coerce

-- | The @ClusterID@ for which specified steps will be canceled. Use 'RunJobFlow' and 'ListClusters' to get ClusterIDs.
csClusterId :: Lens' CancelSteps (Maybe Text)
csClusterId = lens _csClusterId (\ s a -> s{_csClusterId = a})

instance AWSRequest CancelSteps where
        type Rs CancelSteps = CancelStepsResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 CancelStepsResponse' <$>
                   (x .?> "CancelStepsInfoList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CancelSteps where

instance NFData CancelSteps where

instance ToHeaders CancelSteps where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.CancelSteps" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelSteps where
        toJSON CancelSteps'{..}
          = object
              (catMaybes
                 [("StepIds" .=) <$> _csStepIds,
                  ("ClusterId" .=) <$> _csClusterId])

instance ToPath CancelSteps where
        toPath = const "/"

instance ToQuery CancelSteps where
        toQuery = const mempty

-- | The output for the 'CancelSteps' operation.
--
--
--
-- /See:/ 'cancelStepsResponse' smart constructor.
data CancelStepsResponse = CancelStepsResponse'
  { _csrsCancelStepsInfoList :: !(Maybe [CancelStepsInfo])
  , _csrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelStepsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsCancelStepsInfoList' - A list of 'CancelStepsInfo' , which shows the status of specified cancel requests for each @StepID@ specified.
--
-- * 'csrsResponseStatus' - -- | The response status code.
cancelStepsResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CancelStepsResponse
cancelStepsResponse pResponseStatus_ =
  CancelStepsResponse'
    {_csrsCancelStepsInfoList = Nothing, _csrsResponseStatus = pResponseStatus_}


-- | A list of 'CancelStepsInfo' , which shows the status of specified cancel requests for each @StepID@ specified.
csrsCancelStepsInfoList :: Lens' CancelStepsResponse [CancelStepsInfo]
csrsCancelStepsInfoList = lens _csrsCancelStepsInfoList (\ s a -> s{_csrsCancelStepsInfoList = a}) . _Default . _Coerce

-- | -- | The response status code.
csrsResponseStatus :: Lens' CancelStepsResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CancelStepsResponse where
