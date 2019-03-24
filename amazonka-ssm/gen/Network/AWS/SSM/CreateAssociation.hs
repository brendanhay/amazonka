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
-- Module      : Network.AWS.SSM.CreateAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified Systems Manager document with the specified instances or targets.
--
--
-- When you associate a document with one or more instances using instance IDs or tags, SSM Agent running on the instance processes the document and configures the instance as specified.
--
-- If you associate a document with an instance that already has an associated document, the system returns the AssociationAlreadyExists exception.
--
module Network.AWS.SSM.CreateAssociation
    (
    -- * Creating a Request
      createAssociation
    , CreateAssociation
    -- * Request Lenses
    , caInstanceId
    , caMaxErrors
    , caScheduleExpression
    , caOutputLocation
    , caTargets
    , caParameters
    , caDocumentVersion
    , caAutomationTargetParameterName
    , caAssociationName
    , caComplianceSeverity
    , caMaxConcurrency
    , caName

    -- * Destructuring the Response
    , createAssociationResponse
    , CreateAssociationResponse
    -- * Response Lenses
    , crsAssociationDescription
    , crsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'createAssociation' smart constructor.
data CreateAssociation = CreateAssociation'
  { _caInstanceId :: !(Maybe Text)
  , _caMaxErrors :: !(Maybe Text)
  , _caScheduleExpression :: !(Maybe Text)
  , _caOutputLocation :: !(Maybe InstanceAssociationOutputLocation)
  , _caTargets :: !(Maybe [Target])
  , _caParameters :: !(Maybe (Map Text [Text]))
  , _caDocumentVersion :: !(Maybe Text)
  , _caAutomationTargetParameterName :: !(Maybe Text)
  , _caAssociationName :: !(Maybe Text)
  , _caComplianceSeverity :: !(Maybe AssociationComplianceSeverity)
  , _caMaxConcurrency :: !(Maybe Text)
  , _caName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caInstanceId' - The instance ID.
--
-- * 'caMaxErrors' - The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received. Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- * 'caScheduleExpression' - A cron expression when the association will be applied to the target(s).
--
-- * 'caOutputLocation' - An Amazon S3 bucket where you want to store the output details of the request.
--
-- * 'caTargets' - The targets (either instances or tags) for the association.
--
-- * 'caParameters' - The parameters for the documents runtime configuration.
--
-- * 'caDocumentVersion' - The document version you want to associate with the target(s). Can be a specific version or the default version.
--
-- * 'caAutomationTargetParameterName' - Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- * 'caAssociationName' - Specify a descriptive name for the association.
--
-- * 'caComplianceSeverity' - The severity level to assign to the association.
--
-- * 'caMaxConcurrency' - The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time. If a new instance starts and attempts to execute an association while Systems Manager is executing MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- * 'caName' - The name of the SSM document that contains the configuration information for the instance. You can specify Command, Policy, or Automation documents. You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account. For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format: @arn:/partition/ :ssm:/region/ :/account-id/ :document//document-name/ @  For example: @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@  For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
createAssociation
    :: Text -- ^ 'caName'
    -> CreateAssociation
createAssociation pName_ =
  CreateAssociation'
    { _caInstanceId = Nothing
    , _caMaxErrors = Nothing
    , _caScheduleExpression = Nothing
    , _caOutputLocation = Nothing
    , _caTargets = Nothing
    , _caParameters = Nothing
    , _caDocumentVersion = Nothing
    , _caAutomationTargetParameterName = Nothing
    , _caAssociationName = Nothing
    , _caComplianceSeverity = Nothing
    , _caMaxConcurrency = Nothing
    , _caName = pName_
    }


-- | The instance ID.
caInstanceId :: Lens' CreateAssociation (Maybe Text)
caInstanceId = lens _caInstanceId (\ s a -> s{_caInstanceId = a})

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received. Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
caMaxErrors :: Lens' CreateAssociation (Maybe Text)
caMaxErrors = lens _caMaxErrors (\ s a -> s{_caMaxErrors = a})

-- | A cron expression when the association will be applied to the target(s).
caScheduleExpression :: Lens' CreateAssociation (Maybe Text)
caScheduleExpression = lens _caScheduleExpression (\ s a -> s{_caScheduleExpression = a})

-- | An Amazon S3 bucket where you want to store the output details of the request.
caOutputLocation :: Lens' CreateAssociation (Maybe InstanceAssociationOutputLocation)
caOutputLocation = lens _caOutputLocation (\ s a -> s{_caOutputLocation = a})

-- | The targets (either instances or tags) for the association.
caTargets :: Lens' CreateAssociation [Target]
caTargets = lens _caTargets (\ s a -> s{_caTargets = a}) . _Default . _Coerce

-- | The parameters for the documents runtime configuration.
caParameters :: Lens' CreateAssociation (HashMap Text [Text])
caParameters = lens _caParameters (\ s a -> s{_caParameters = a}) . _Default . _Map

-- | The document version you want to associate with the target(s). Can be a specific version or the default version.
caDocumentVersion :: Lens' CreateAssociation (Maybe Text)
caDocumentVersion = lens _caDocumentVersion (\ s a -> s{_caDocumentVersion = a})

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
caAutomationTargetParameterName :: Lens' CreateAssociation (Maybe Text)
caAutomationTargetParameterName = lens _caAutomationTargetParameterName (\ s a -> s{_caAutomationTargetParameterName = a})

-- | Specify a descriptive name for the association.
caAssociationName :: Lens' CreateAssociation (Maybe Text)
caAssociationName = lens _caAssociationName (\ s a -> s{_caAssociationName = a})

-- | The severity level to assign to the association.
caComplianceSeverity :: Lens' CreateAssociation (Maybe AssociationComplianceSeverity)
caComplianceSeverity = lens _caComplianceSeverity (\ s a -> s{_caComplianceSeverity = a})

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time. If a new instance starts and attempts to execute an association while Systems Manager is executing MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
caMaxConcurrency :: Lens' CreateAssociation (Maybe Text)
caMaxConcurrency = lens _caMaxConcurrency (\ s a -> s{_caMaxConcurrency = a})

-- | The name of the SSM document that contains the configuration information for the instance. You can specify Command, Policy, or Automation documents. You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account. For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format: @arn:/partition/ :ssm:/region/ :/account-id/ :document//document-name/ @  For example: @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@  For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
caName :: Lens' CreateAssociation Text
caName = lens _caName (\ s a -> s{_caName = a})

instance AWSRequest CreateAssociation where
        type Rs CreateAssociation = CreateAssociationResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 CreateAssociationResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

instance Hashable CreateAssociation where

instance NFData CreateAssociation where

instance ToHeaders CreateAssociation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CreateAssociation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAssociation where
        toJSON CreateAssociation'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _caInstanceId,
                  ("MaxErrors" .=) <$> _caMaxErrors,
                  ("ScheduleExpression" .=) <$> _caScheduleExpression,
                  ("OutputLocation" .=) <$> _caOutputLocation,
                  ("Targets" .=) <$> _caTargets,
                  ("Parameters" .=) <$> _caParameters,
                  ("DocumentVersion" .=) <$> _caDocumentVersion,
                  ("AutomationTargetParameterName" .=) <$>
                    _caAutomationTargetParameterName,
                  ("AssociationName" .=) <$> _caAssociationName,
                  ("ComplianceSeverity" .=) <$> _caComplianceSeverity,
                  ("MaxConcurrency" .=) <$> _caMaxConcurrency,
                  Just ("Name" .= _caName)])

instance ToPath CreateAssociation where
        toPath = const "/"

instance ToQuery CreateAssociation where
        toQuery = const mempty

-- | /See:/ 'createAssociationResponse' smart constructor.
data CreateAssociationResponse = CreateAssociationResponse'
  { _crsAssociationDescription :: !(Maybe AssociationDescription)
  , _crsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsAssociationDescription' - Information about the association.
--
-- * 'crsResponseStatus' - -- | The response status code.
createAssociationResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateAssociationResponse
createAssociationResponse pResponseStatus_ =
  CreateAssociationResponse'
    { _crsAssociationDescription = Nothing
    , _crsResponseStatus = pResponseStatus_
    }


-- | Information about the association.
crsAssociationDescription :: Lens' CreateAssociationResponse (Maybe AssociationDescription)
crsAssociationDescription = lens _crsAssociationDescription (\ s a -> s{_crsAssociationDescription = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateAssociationResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateAssociationResponse where
