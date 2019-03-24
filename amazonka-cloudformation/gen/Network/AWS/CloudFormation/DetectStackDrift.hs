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
-- Module      : Network.AWS.CloudFormation.DetectStackDrift
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For each resource in the stack that supports drift detection, AWS CloudFormation compares the actual configuration of the resource with its expected template configuration. Only resource properties explicitly defined in the stack template are checked for drift. A stack is considered to have drifted if one or more of its resources differ from their expected template configurations. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
--
-- Use @DetectStackDrift@ to detect drift on all supported resources for a given stack, or 'DetectStackResourceDrift' to detect drift on individual resources.
--
-- For a list of stack resources that currently support drift detection, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
--
-- @DetectStackDrift@ can take up to several minutes, depending on the number of resources contained within the stack. Use 'DescribeStackDriftDetectionStatus' to monitor the progress of a detect stack drift operation. Once the drift detection operation has completed, use 'DescribeStackResourceDrifts' to return drift information about the stack and its resources.
--
-- When detecting drift on a stack, AWS CloudFormation does not detect drift on any nested stacks belonging to that stack. Perform @DetectStackDrift@ directly on the nested stack itself.
--
module Network.AWS.CloudFormation.DetectStackDrift
    (
    -- * Creating a Request
      detectStackDrift
    , DetectStackDrift
    -- * Request Lenses
    , dsdLogicalResourceIds
    , dsdStackName

    -- * Destructuring the Response
    , detectStackDriftResponse
    , DetectStackDriftResponse
    -- * Response Lenses
    , dsdrsResponseStatus
    , dsdrsStackDriftDetectionId
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectStackDrift' smart constructor.
data DetectStackDrift = DetectStackDrift'
  { _dsdLogicalResourceIds :: !(Maybe (List1 Text))
  , _dsdStackName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectStackDrift' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdLogicalResourceIds' - The logical names of any resources you want to use as filters.
--
-- * 'dsdStackName' - The name of the stack for which you want to detect drift.
detectStackDrift
    :: Text -- ^ 'dsdStackName'
    -> DetectStackDrift
detectStackDrift pStackName_ =
  DetectStackDrift'
    {_dsdLogicalResourceIds = Nothing, _dsdStackName = pStackName_}


-- | The logical names of any resources you want to use as filters.
dsdLogicalResourceIds :: Lens' DetectStackDrift (Maybe (NonEmpty Text))
dsdLogicalResourceIds = lens _dsdLogicalResourceIds (\ s a -> s{_dsdLogicalResourceIds = a}) . mapping _List1

-- | The name of the stack for which you want to detect drift.
dsdStackName :: Lens' DetectStackDrift Text
dsdStackName = lens _dsdStackName (\ s a -> s{_dsdStackName = a})

instance AWSRequest DetectStackDrift where
        type Rs DetectStackDrift = DetectStackDriftResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DetectStackDriftResult"
              (\ s h x ->
                 DetectStackDriftResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@ "StackDriftDetectionId"))

instance Hashable DetectStackDrift where

instance NFData DetectStackDrift where

instance ToHeaders DetectStackDrift where
        toHeaders = const mempty

instance ToPath DetectStackDrift where
        toPath = const "/"

instance ToQuery DetectStackDrift where
        toQuery DetectStackDrift'{..}
          = mconcat
              ["Action" =: ("DetectStackDrift" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "LogicalResourceIds" =:
                 toQuery
                   (toQueryList "member" <$> _dsdLogicalResourceIds),
               "StackName" =: _dsdStackName]

-- | /See:/ 'detectStackDriftResponse' smart constructor.
data DetectStackDriftResponse = DetectStackDriftResponse'
  { _dsdrsResponseStatus        :: !Int
  , _dsdrsStackDriftDetectionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectStackDriftResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdrsResponseStatus' - -- | The response status code.
--
-- * 'dsdrsStackDriftDetectionId' - The ID of the drift detection results of this operation.  AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
detectStackDriftResponse
    :: Int -- ^ 'dsdrsResponseStatus'
    -> Text -- ^ 'dsdrsStackDriftDetectionId'
    -> DetectStackDriftResponse
detectStackDriftResponse pResponseStatus_ pStackDriftDetectionId_ =
  DetectStackDriftResponse'
    { _dsdrsResponseStatus = pResponseStatus_
    , _dsdrsStackDriftDetectionId = pStackDriftDetectionId_
    }


-- | -- | The response status code.
dsdrsResponseStatus :: Lens' DetectStackDriftResponse Int
dsdrsResponseStatus = lens _dsdrsResponseStatus (\ s a -> s{_dsdrsResponseStatus = a})

-- | The ID of the drift detection results of this operation.  AWS CloudFormation generates new results, with a new drift detection ID, each time this operation is run. However, the number of drift results AWS CloudFormation retains for any given stack, and for how long, may vary.
dsdrsStackDriftDetectionId :: Lens' DetectStackDriftResponse Text
dsdrsStackDriftDetectionId = lens _dsdrsStackDriftDetectionId (\ s a -> s{_dsdrsStackDriftDetectionId = a})

instance NFData DetectStackDriftResponse where
