{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.SetTerminationProtection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- SetTerminationProtection locks a job flow so the Amazon EC2 instances in
-- the cluster cannot be terminated by user intervention, an API call, or
-- in the event of a job-flow error. The cluster still terminates upon
-- successful completion of the job flow. Calling SetTerminationProtection
-- on a job flow is analogous to calling the Amazon EC2
-- DisableAPITermination API on all of the EC2 instances in a cluster.
--
-- SetTerminationProtection is used to prevent accidental termination of a
-- job flow and to ensure that in the event of an error, the instances will
-- persist so you can recover any data stored in their ephemeral instance
-- storage.
--
-- To terminate a job flow that has been locked by setting
-- SetTerminationProtection to @true@, you must first unlock the job flow
-- by a subsequent call to SetTerminationProtection in which you set the
-- value to @false@.
--
-- For more information, go to
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/UsingEMR_TerminationProtection.html Protecting a Job Flow from Termination>
-- in the /Amazon Elastic MapReduce Developer\'s Guide./
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_SetTerminationProtection.html>
module Network.AWS.EMR.SetTerminationProtection
    (
    -- * Request
      SetTerminationProtection
    -- ** Request constructor
    , setTerminationProtection
    -- ** Request lenses
    , stprqJobFlowIds
    , stprqTerminationProtected

    -- * Response
    , SetTerminationProtectionResponse
    -- ** Response constructor
    , setTerminationProtectionResponse
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input argument to the TerminationProtection operation.
--
-- /See:/ 'setTerminationProtection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stprqJobFlowIds'
--
-- * 'stprqTerminationProtected'
data SetTerminationProtection = SetTerminationProtection'
    { _stprqJobFlowIds           :: ![Text]
    , _stprqTerminationProtected :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetTerminationProtection' smart constructor.
setTerminationProtection :: Bool -> SetTerminationProtection
setTerminationProtection pTerminationProtected =
    SetTerminationProtection'
    { _stprqJobFlowIds = mempty
    , _stprqTerminationProtected = pTerminationProtected
    }

-- | A list of strings that uniquely identify the job flows to protect. This
-- identifier is returned by RunJobFlow and can also be obtained from
-- DescribeJobFlows .
stprqJobFlowIds :: Lens' SetTerminationProtection [Text]
stprqJobFlowIds = lens _stprqJobFlowIds (\ s a -> s{_stprqJobFlowIds = a});

-- | A Boolean that indicates whether to protect the job flow and prevent the
-- Amazon EC2 instances in the cluster from shutting down due to API calls,
-- user intervention, or job-flow error.
stprqTerminationProtected :: Lens' SetTerminationProtection Bool
stprqTerminationProtected = lens _stprqTerminationProtected (\ s a -> s{_stprqTerminationProtected = a});

instance AWSRequest SetTerminationProtection where
        type Sv SetTerminationProtection = EMR
        type Rs SetTerminationProtection =
             SetTerminationProtectionResponse
        request = postJSON
        response
          = receiveNull SetTerminationProtectionResponse'

instance ToHeaders SetTerminationProtection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.SetTerminationProtection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetTerminationProtection where
        toJSON SetTerminationProtection'{..}
          = object
              ["JobFlowIds" .= _stprqJobFlowIds,
               "TerminationProtected" .= _stprqTerminationProtected]

instance ToPath SetTerminationProtection where
        toPath = const "/"

instance ToQuery SetTerminationProtection where
        toQuery = const mempty

-- | /See:/ 'setTerminationProtectionResponse' smart constructor.
data SetTerminationProtectionResponse =
    SetTerminationProtectionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetTerminationProtectionResponse' smart constructor.
setTerminationProtectionResponse :: SetTerminationProtectionResponse
setTerminationProtectionResponse = SetTerminationProtectionResponse'
