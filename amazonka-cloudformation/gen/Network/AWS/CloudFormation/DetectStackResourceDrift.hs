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
-- Module      : Network.AWS.CloudFormation.DetectStackResourceDrift
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about whether a resource's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. This information includes actual and expected property values for resources in which AWS CloudFormation detects drift. Only resource properties explicitly defined in the stack template are checked for drift. For more information about stack and resource drift, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
--
-- Use @DetectStackResourceDrift@ to detect drift on individual resources, or 'DetectStackDrift' to detect drift on all resources in a given stack that support drift detection.
--
-- Resources that do not currently support drift detection cannot be checked. For a list of resources that support drift detection, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
--
module Network.AWS.CloudFormation.DetectStackResourceDrift
    (
    -- * Creating a Request
      detectStackResourceDrift
    , DetectStackResourceDrift
    -- * Request Lenses
    , detStackName
    , detLogicalResourceId

    -- * Destructuring the Response
    , detectStackResourceDriftResponse
    , DetectStackResourceDriftResponse
    -- * Response Lenses
    , dsrdrsResponseStatus
    , dsrdrsStackResourceDrift
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectStackResourceDrift' smart constructor.
data DetectStackResourceDrift = DetectStackResourceDrift'
  { _detStackName         :: !Text
  , _detLogicalResourceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectStackResourceDrift' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detStackName' - The name of the stack to which the resource belongs.
--
-- * 'detLogicalResourceId' - The logical name of the resource for which to return drift information.
detectStackResourceDrift
    :: Text -- ^ 'detStackName'
    -> Text -- ^ 'detLogicalResourceId'
    -> DetectStackResourceDrift
detectStackResourceDrift pStackName_ pLogicalResourceId_ =
  DetectStackResourceDrift'
    {_detStackName = pStackName_, _detLogicalResourceId = pLogicalResourceId_}


-- | The name of the stack to which the resource belongs.
detStackName :: Lens' DetectStackResourceDrift Text
detStackName = lens _detStackName (\ s a -> s{_detStackName = a})

-- | The logical name of the resource for which to return drift information.
detLogicalResourceId :: Lens' DetectStackResourceDrift Text
detLogicalResourceId = lens _detLogicalResourceId (\ s a -> s{_detLogicalResourceId = a})

instance AWSRequest DetectStackResourceDrift where
        type Rs DetectStackResourceDrift =
             DetectStackResourceDriftResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DetectStackResourceDriftResult"
              (\ s h x ->
                 DetectStackResourceDriftResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "StackResourceDrift"))

instance Hashable DetectStackResourceDrift where

instance NFData DetectStackResourceDrift where

instance ToHeaders DetectStackResourceDrift where
        toHeaders = const mempty

instance ToPath DetectStackResourceDrift where
        toPath = const "/"

instance ToQuery DetectStackResourceDrift where
        toQuery DetectStackResourceDrift'{..}
          = mconcat
              ["Action" =:
                 ("DetectStackResourceDrift" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _detStackName,
               "LogicalResourceId" =: _detLogicalResourceId]

-- | /See:/ 'detectStackResourceDriftResponse' smart constructor.
data DetectStackResourceDriftResponse = DetectStackResourceDriftResponse'
  { _dsrdrsResponseStatus     :: !Int
  , _dsrdrsStackResourceDrift :: !StackResourceDrift
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectStackResourceDriftResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrdrsResponseStatus' - -- | The response status code.
--
-- * 'dsrdrsStackResourceDrift' - Information about whether the resource's actual configuration has drifted from its expected template configuration, including actual and expected property values and any differences detected.
detectStackResourceDriftResponse
    :: Int -- ^ 'dsrdrsResponseStatus'
    -> StackResourceDrift -- ^ 'dsrdrsStackResourceDrift'
    -> DetectStackResourceDriftResponse
detectStackResourceDriftResponse pResponseStatus_ pStackResourceDrift_ =
  DetectStackResourceDriftResponse'
    { _dsrdrsResponseStatus = pResponseStatus_
    , _dsrdrsStackResourceDrift = pStackResourceDrift_
    }


-- | -- | The response status code.
dsrdrsResponseStatus :: Lens' DetectStackResourceDriftResponse Int
dsrdrsResponseStatus = lens _dsrdrsResponseStatus (\ s a -> s{_dsrdrsResponseStatus = a})

-- | Information about whether the resource's actual configuration has drifted from its expected template configuration, including actual and expected property values and any differences detected.
dsrdrsStackResourceDrift :: Lens' DetectStackResourceDriftResponse StackResourceDrift
dsrdrsStackResourceDrift = lens _dsrdrsStackResourceDrift (\ s a -> s{_dsrdrsStackResourceDrift = a})

instance NFData DetectStackResourceDriftResponse
         where
