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
-- Module      : Network.AWS.EMR.PutAutoScalingPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric.
--
--
module Network.AWS.EMR.PutAutoScalingPolicy
    (
    -- * Creating a Request
      putAutoScalingPolicy
    , PutAutoScalingPolicy
    -- * Request Lenses
    , paspClusterId
    , paspInstanceGroupId
    , paspAutoScalingPolicy

    -- * Destructuring the Response
    , putAutoScalingPolicyResponse
    , PutAutoScalingPolicyResponse
    -- * Response Lenses
    , pasprsClusterId
    , pasprsAutoScalingPolicy
    , pasprsInstanceGroupId
    , pasprsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putAutoScalingPolicy' smart constructor.
data PutAutoScalingPolicy = PutAutoScalingPolicy'
  { _paspClusterId         :: !Text
  , _paspInstanceGroupId   :: !Text
  , _paspAutoScalingPolicy :: !AutoScalingPolicy
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAutoScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paspClusterId' - Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
--
-- * 'paspInstanceGroupId' - Specifies the ID of the instance group to which the automatic scaling policy is applied.
--
-- * 'paspAutoScalingPolicy' - Specifies the definition of the automatic scaling policy.
putAutoScalingPolicy
    :: Text -- ^ 'paspClusterId'
    -> Text -- ^ 'paspInstanceGroupId'
    -> AutoScalingPolicy -- ^ 'paspAutoScalingPolicy'
    -> PutAutoScalingPolicy
putAutoScalingPolicy pClusterId_ pInstanceGroupId_ pAutoScalingPolicy_ =
  PutAutoScalingPolicy'
    { _paspClusterId = pClusterId_
    , _paspInstanceGroupId = pInstanceGroupId_
    , _paspAutoScalingPolicy = pAutoScalingPolicy_
    }


-- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
paspClusterId :: Lens' PutAutoScalingPolicy Text
paspClusterId = lens _paspClusterId (\ s a -> s{_paspClusterId = a})

-- | Specifies the ID of the instance group to which the automatic scaling policy is applied.
paspInstanceGroupId :: Lens' PutAutoScalingPolicy Text
paspInstanceGroupId = lens _paspInstanceGroupId (\ s a -> s{_paspInstanceGroupId = a})

-- | Specifies the definition of the automatic scaling policy.
paspAutoScalingPolicy :: Lens' PutAutoScalingPolicy AutoScalingPolicy
paspAutoScalingPolicy = lens _paspAutoScalingPolicy (\ s a -> s{_paspAutoScalingPolicy = a})

instance AWSRequest PutAutoScalingPolicy where
        type Rs PutAutoScalingPolicy =
             PutAutoScalingPolicyResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 PutAutoScalingPolicyResponse' <$>
                   (x .?> "ClusterId") <*> (x .?> "AutoScalingPolicy")
                     <*> (x .?> "InstanceGroupId")
                     <*> (pure (fromEnum s)))

instance Hashable PutAutoScalingPolicy where

instance NFData PutAutoScalingPolicy where

instance ToHeaders PutAutoScalingPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.PutAutoScalingPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutAutoScalingPolicy where
        toJSON PutAutoScalingPolicy'{..}
          = object
              (catMaybes
                 [Just ("ClusterId" .= _paspClusterId),
                  Just ("InstanceGroupId" .= _paspInstanceGroupId),
                  Just
                    ("AutoScalingPolicy" .= _paspAutoScalingPolicy)])

instance ToPath PutAutoScalingPolicy where
        toPath = const "/"

instance ToQuery PutAutoScalingPolicy where
        toQuery = const mempty

-- | /See:/ 'putAutoScalingPolicyResponse' smart constructor.
data PutAutoScalingPolicyResponse = PutAutoScalingPolicyResponse'
  { _pasprsClusterId         :: !(Maybe Text)
  , _pasprsAutoScalingPolicy :: !(Maybe AutoScalingPolicyDescription)
  , _pasprsInstanceGroupId   :: !(Maybe Text)
  , _pasprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAutoScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pasprsClusterId' - Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
--
-- * 'pasprsAutoScalingPolicy' - The automatic scaling policy definition.
--
-- * 'pasprsInstanceGroupId' - Specifies the ID of the instance group to which the scaling policy is applied.
--
-- * 'pasprsResponseStatus' - -- | The response status code.
putAutoScalingPolicyResponse
    :: Int -- ^ 'pasprsResponseStatus'
    -> PutAutoScalingPolicyResponse
putAutoScalingPolicyResponse pResponseStatus_ =
  PutAutoScalingPolicyResponse'
    { _pasprsClusterId = Nothing
    , _pasprsAutoScalingPolicy = Nothing
    , _pasprsInstanceGroupId = Nothing
    , _pasprsResponseStatus = pResponseStatus_
    }


-- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
pasprsClusterId :: Lens' PutAutoScalingPolicyResponse (Maybe Text)
pasprsClusterId = lens _pasprsClusterId (\ s a -> s{_pasprsClusterId = a})

-- | The automatic scaling policy definition.
pasprsAutoScalingPolicy :: Lens' PutAutoScalingPolicyResponse (Maybe AutoScalingPolicyDescription)
pasprsAutoScalingPolicy = lens _pasprsAutoScalingPolicy (\ s a -> s{_pasprsAutoScalingPolicy = a})

-- | Specifies the ID of the instance group to which the scaling policy is applied.
pasprsInstanceGroupId :: Lens' PutAutoScalingPolicyResponse (Maybe Text)
pasprsInstanceGroupId = lens _pasprsInstanceGroupId (\ s a -> s{_pasprsInstanceGroupId = a})

-- | -- | The response status code.
pasprsResponseStatus :: Lens' PutAutoScalingPolicyResponse Int
pasprsResponseStatus = lens _pasprsResponseStatus (\ s a -> s{_pasprsResponseStatus = a})

instance NFData PutAutoScalingPolicyResponse where
