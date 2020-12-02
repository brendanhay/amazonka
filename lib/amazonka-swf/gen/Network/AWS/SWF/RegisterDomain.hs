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
-- Module      : Network.AWS.SWF.RegisterDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new domain.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * You cannot use an IAM policy to control domain access for this action. The name of the domain being registered is available as the resource of this action.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.RegisterDomain
    (
    -- * Creating a Request
      registerDomain
    , RegisterDomain
    -- * Request Lenses
    , rdDescription
    , rdName
    , rdWorkflowExecutionRetentionPeriodInDays

    -- * Destructuring the Response
    , registerDomainResponse
    , RegisterDomainResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'registerDomain' smart constructor.
data RegisterDomain = RegisterDomain'
  { _rdDescription                            :: !(Maybe Text)
  , _rdName                                   :: !Text
  , _rdWorkflowExecutionRetentionPeriodInDays :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdDescription' - A text description of the domain.
--
-- * 'rdName' - Name of the domain to register. The name must be unique in the region that the domain is registered in. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- * 'rdWorkflowExecutionRetentionPeriodInDays' - The duration (in days) that records and histories of workflow executions on the domain should be kept by the service. After the retention period, the workflow execution isn't available in the results of visibility calls. If you pass the value @NONE@ or @0@ (zero), then the workflow execution history isn't retained. As soon as the workflow execution completes, the execution record and its history are deleted. The maximum workflow execution retention period is 90 days. For more information about Amazon SWF service limits, see: <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits> in the /Amazon SWF Developer Guide/ .
registerDomain
    :: Text -- ^ 'rdName'
    -> Text -- ^ 'rdWorkflowExecutionRetentionPeriodInDays'
    -> RegisterDomain
registerDomain pName_ pWorkflowExecutionRetentionPeriodInDays_ =
  RegisterDomain'
    { _rdDescription = Nothing
    , _rdName = pName_
    , _rdWorkflowExecutionRetentionPeriodInDays =
        pWorkflowExecutionRetentionPeriodInDays_
    }


-- | A text description of the domain.
rdDescription :: Lens' RegisterDomain (Maybe Text)
rdDescription = lens _rdDescription (\ s a -> s{_rdDescription = a})

-- | Name of the domain to register. The name must be unique in the region that the domain is registered in. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
rdName :: Lens' RegisterDomain Text
rdName = lens _rdName (\ s a -> s{_rdName = a})

-- | The duration (in days) that records and histories of workflow executions on the domain should be kept by the service. After the retention period, the workflow execution isn't available in the results of visibility calls. If you pass the value @NONE@ or @0@ (zero), then the workflow execution history isn't retained. As soon as the workflow execution completes, the execution record and its history are deleted. The maximum workflow execution retention period is 90 days. For more information about Amazon SWF service limits, see: <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits> in the /Amazon SWF Developer Guide/ .
rdWorkflowExecutionRetentionPeriodInDays :: Lens' RegisterDomain Text
rdWorkflowExecutionRetentionPeriodInDays = lens _rdWorkflowExecutionRetentionPeriodInDays (\ s a -> s{_rdWorkflowExecutionRetentionPeriodInDays = a})

instance AWSRequest RegisterDomain where
        type Rs RegisterDomain = RegisterDomainResponse
        request = postJSON swf
        response = receiveNull RegisterDomainResponse'

instance Hashable RegisterDomain where

instance NFData RegisterDomain where

instance ToHeaders RegisterDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.RegisterDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON RegisterDomain where
        toJSON RegisterDomain'{..}
          = object
              (catMaybes
                 [("description" .=) <$> _rdDescription,
                  Just ("name" .= _rdName),
                  Just
                    ("workflowExecutionRetentionPeriodInDays" .=
                       _rdWorkflowExecutionRetentionPeriodInDays)])

instance ToPath RegisterDomain where
        toPath = const "/"

instance ToQuery RegisterDomain where
        toQuery = const mempty

-- | /See:/ 'registerDomainResponse' smart constructor.
data RegisterDomainResponse =
  RegisterDomainResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterDomainResponse' with the minimum fields required to make a request.
--
registerDomainResponse
    :: RegisterDomainResponse
registerDomainResponse = RegisterDomainResponse'


instance NFData RegisterDomainResponse where
