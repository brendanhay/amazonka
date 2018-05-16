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
-- Module      : Network.AWS.EMR.SetTerminationProtection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- SetTerminationProtection locks a cluster (job flow) so the EC2 instances in the cluster cannot be terminated by user intervention, an API call, or in the event of a job-flow error. The cluster still terminates upon successful completion of the job flow. Calling @SetTerminationProtection@ on a cluster is similar to calling the Amazon EC2 @DisableAPITermination@ API on all EC2 instances in a cluster.
--
--
-- @SetTerminationProtection@ is used to prevent accidental termination of a cluster and to ensure that in the event of an error, the instances persist so that you can recover any data stored in their ephemeral instance storage.
--
-- To terminate a cluster that has been locked by setting @SetTerminationProtection@ to @true@ , you must first unlock the job flow by a subsequent call to @SetTerminationProtection@ in which you set the value to @false@ .
--
-- For more information, see<http://docs.aws.amazon.com/emr/latest/ManagementGuide/UsingEMR_TerminationProtection.html Managing Cluster Termination> in the /Amazon EMR Management Guide/ .
--
module Network.AWS.EMR.SetTerminationProtection
    (
    -- * Creating a Request
      setTerminationProtection
    , SetTerminationProtection
    -- * Request Lenses
    , stpJobFlowIds
    , stpTerminationProtected

    -- * Destructuring the Response
    , setTerminationProtectionResponse
    , SetTerminationProtectionResponse
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input argument to the 'TerminationProtection' operation.
--
--
--
-- /See:/ 'setTerminationProtection' smart constructor.
data SetTerminationProtection = SetTerminationProtection'
  { _stpJobFlowIds           :: ![Text]
  , _stpTerminationProtected :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetTerminationProtection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stpJobFlowIds' - A list of strings that uniquely identify the clusters to protect. This identifier is returned by 'RunJobFlow' and can also be obtained from 'DescribeJobFlows' .
--
-- * 'stpTerminationProtected' - A Boolean that indicates whether to protect the cluster and prevent the Amazon EC2 instances in the cluster from shutting down due to API calls, user intervention, or job-flow error.
setTerminationProtection
    :: Bool -- ^ 'stpTerminationProtected'
    -> SetTerminationProtection
setTerminationProtection pTerminationProtected_ =
  SetTerminationProtection'
    {_stpJobFlowIds = mempty, _stpTerminationProtected = pTerminationProtected_}


-- | A list of strings that uniquely identify the clusters to protect. This identifier is returned by 'RunJobFlow' and can also be obtained from 'DescribeJobFlows' .
stpJobFlowIds :: Lens' SetTerminationProtection [Text]
stpJobFlowIds = lens _stpJobFlowIds (\ s a -> s{_stpJobFlowIds = a}) . _Coerce

-- | A Boolean that indicates whether to protect the cluster and prevent the Amazon EC2 instances in the cluster from shutting down due to API calls, user intervention, or job-flow error.
stpTerminationProtected :: Lens' SetTerminationProtection Bool
stpTerminationProtected = lens _stpTerminationProtected (\ s a -> s{_stpTerminationProtected = a})

instance AWSRequest SetTerminationProtection where
        type Rs SetTerminationProtection =
             SetTerminationProtectionResponse
        request = postJSON emr
        response
          = receiveNull SetTerminationProtectionResponse'

instance Hashable SetTerminationProtection where

instance NFData SetTerminationProtection where

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
              (catMaybes
                 [Just ("JobFlowIds" .= _stpJobFlowIds),
                  Just
                    ("TerminationProtected" .=
                       _stpTerminationProtected)])

instance ToPath SetTerminationProtection where
        toPath = const "/"

instance ToQuery SetTerminationProtection where
        toQuery = const mempty

-- | /See:/ 'setTerminationProtectionResponse' smart constructor.
data SetTerminationProtectionResponse =
  SetTerminationProtectionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetTerminationProtectionResponse' with the minimum fields required to make a request.
--
setTerminationProtectionResponse
    :: SetTerminationProtectionResponse
setTerminationProtectionResponse = SetTerminationProtectionResponse'


instance NFData SetTerminationProtectionResponse
         where
