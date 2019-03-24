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
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateCloudFormationChangeSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation change set for the given application.
--
--
module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationChangeSet
    (
    -- * Creating a Request
      createCloudFormationChangeSet
    , CreateCloudFormationChangeSet
    -- * Request Lenses
    , ccfcsClientToken
    , ccfcsTemplateId
    , ccfcsSemanticVersion
    , ccfcsNotificationARNs
    , ccfcsChangeSetName
    , ccfcsDescription
    , ccfcsCapabilities
    , ccfcsParameterOverrides
    , ccfcsRollbackConfiguration
    , ccfcsResourceTypes
    , ccfcsTags
    , ccfcsApplicationId
    , ccfcsStackName

    -- * Destructuring the Response
    , createCloudFormationChangeSetResponse
    , CreateCloudFormationChangeSetResponse
    -- * Response Lenses
    , ccfcsrsSemanticVersion
    , ccfcsrsChangeSetId
    , ccfcsrsApplicationId
    , ccfcsrsStackId
    , ccfcsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'createCloudFormationChangeSet' smart constructor.
data CreateCloudFormationChangeSet = CreateCloudFormationChangeSet'
  { _ccfcsClientToken           :: !(Maybe Text)
  , _ccfcsTemplateId            :: !(Maybe Text)
  , _ccfcsSemanticVersion       :: !(Maybe Text)
  , _ccfcsNotificationARNs      :: !(Maybe [Text])
  , _ccfcsChangeSetName         :: !(Maybe Text)
  , _ccfcsDescription           :: !(Maybe Text)
  , _ccfcsCapabilities          :: !(Maybe [Text])
  , _ccfcsParameterOverrides    :: !(Maybe [ParameterValue])
  , _ccfcsRollbackConfiguration :: !(Maybe RollbackConfiguration)
  , _ccfcsResourceTypes         :: !(Maybe [Text])
  , _ccfcsTags                  :: !(Maybe [Tag])
  , _ccfcsApplicationId         :: !Text
  , _ccfcsStackName             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCloudFormationChangeSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfcsClientToken' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- * 'ccfcsTemplateId' - The UUID returned by CreateCloudFormationTemplate. Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- * 'ccfcsSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
--
-- * 'ccfcsNotificationARNs' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- * 'ccfcsChangeSetName' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- * 'ccfcsDescription' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- * 'ccfcsCapabilities' - A list of values that you must specify before you can deploy certain applications.  Some applications might include resources that can affect permissions in your AWS  account, for example, by creating new AWS Identity and Access Management (IAM) users.  For those applications, you must explicitly acknowledge their capabilities by  specifying this parameter. The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND. The following resources require you to specify CAPABILITY_IAM or  CAPABILITY_NAMED_IAM:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .  If the application contains IAM resources, you can specify either CAPABILITY_IAM  or CAPABILITY_NAMED_IAM. If the application contains IAM resources  with custom names, you must specify CAPABILITY_NAMED_IAM. The following resources require you to specify CAPABILITY_RESOURCE_POLICY:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS:TopicPolicy> . Applications that contain one or more nested applications require you to specify  CAPABILITY_AUTO_EXPAND. If your application template contains any of the above resources, we recommend that you review  all permissions associated with the application before deploying. If you don't specify  this parameter for an application that requires capabilities, the call will fail.
--
-- * 'ccfcsParameterOverrides' - A list of parameter values for the parameters of the application.
--
-- * 'ccfcsRollbackConfiguration' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- * 'ccfcsResourceTypes' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- * 'ccfcsTags' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- * 'ccfcsApplicationId' - The Amazon Resource Name (ARN) of the application.
--
-- * 'ccfcsStackName' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
createCloudFormationChangeSet
    :: Text -- ^ 'ccfcsApplicationId'
    -> Text -- ^ 'ccfcsStackName'
    -> CreateCloudFormationChangeSet
createCloudFormationChangeSet pApplicationId_ pStackName_ =
  CreateCloudFormationChangeSet'
    { _ccfcsClientToken = Nothing
    , _ccfcsTemplateId = Nothing
    , _ccfcsSemanticVersion = Nothing
    , _ccfcsNotificationARNs = Nothing
    , _ccfcsChangeSetName = Nothing
    , _ccfcsDescription = Nothing
    , _ccfcsCapabilities = Nothing
    , _ccfcsParameterOverrides = Nothing
    , _ccfcsRollbackConfiguration = Nothing
    , _ccfcsResourceTypes = Nothing
    , _ccfcsTags = Nothing
    , _ccfcsApplicationId = pApplicationId_
    , _ccfcsStackName = pStackName_
    }


-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
ccfcsClientToken :: Lens' CreateCloudFormationChangeSet (Maybe Text)
ccfcsClientToken = lens _ccfcsClientToken (\ s a -> s{_ccfcsClientToken = a})

-- | The UUID returned by CreateCloudFormationTemplate. Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
ccfcsTemplateId :: Lens' CreateCloudFormationChangeSet (Maybe Text)
ccfcsTemplateId = lens _ccfcsTemplateId (\ s a -> s{_ccfcsTemplateId = a})

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
ccfcsSemanticVersion :: Lens' CreateCloudFormationChangeSet (Maybe Text)
ccfcsSemanticVersion = lens _ccfcsSemanticVersion (\ s a -> s{_ccfcsSemanticVersion = a})

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
ccfcsNotificationARNs :: Lens' CreateCloudFormationChangeSet [Text]
ccfcsNotificationARNs = lens _ccfcsNotificationARNs (\ s a -> s{_ccfcsNotificationARNs = a}) . _Default . _Coerce

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
ccfcsChangeSetName :: Lens' CreateCloudFormationChangeSet (Maybe Text)
ccfcsChangeSetName = lens _ccfcsChangeSetName (\ s a -> s{_ccfcsChangeSetName = a})

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
ccfcsDescription :: Lens' CreateCloudFormationChangeSet (Maybe Text)
ccfcsDescription = lens _ccfcsDescription (\ s a -> s{_ccfcsDescription = a})

-- | A list of values that you must specify before you can deploy certain applications.  Some applications might include resources that can affect permissions in your AWS  account, for example, by creating new AWS Identity and Access Management (IAM) users.  For those applications, you must explicitly acknowledge their capabilities by  specifying this parameter. The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND. The following resources require you to specify CAPABILITY_IAM or  CAPABILITY_NAMED_IAM:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .  If the application contains IAM resources, you can specify either CAPABILITY_IAM  or CAPABILITY_NAMED_IAM. If the application contains IAM resources  with custom names, you must specify CAPABILITY_NAMED_IAM. The following resources require you to specify CAPABILITY_RESOURCE_POLICY:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS:TopicPolicy> . Applications that contain one or more nested applications require you to specify  CAPABILITY_AUTO_EXPAND. If your application template contains any of the above resources, we recommend that you review  all permissions associated with the application before deploying. If you don't specify  this parameter for an application that requires capabilities, the call will fail.
ccfcsCapabilities :: Lens' CreateCloudFormationChangeSet [Text]
ccfcsCapabilities = lens _ccfcsCapabilities (\ s a -> s{_ccfcsCapabilities = a}) . _Default . _Coerce

-- | A list of parameter values for the parameters of the application.
ccfcsParameterOverrides :: Lens' CreateCloudFormationChangeSet [ParameterValue]
ccfcsParameterOverrides = lens _ccfcsParameterOverrides (\ s a -> s{_ccfcsParameterOverrides = a}) . _Default . _Coerce

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
ccfcsRollbackConfiguration :: Lens' CreateCloudFormationChangeSet (Maybe RollbackConfiguration)
ccfcsRollbackConfiguration = lens _ccfcsRollbackConfiguration (\ s a -> s{_ccfcsRollbackConfiguration = a})

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
ccfcsResourceTypes :: Lens' CreateCloudFormationChangeSet [Text]
ccfcsResourceTypes = lens _ccfcsResourceTypes (\ s a -> s{_ccfcsResourceTypes = a}) . _Default . _Coerce

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
ccfcsTags :: Lens' CreateCloudFormationChangeSet [Tag]
ccfcsTags = lens _ccfcsTags (\ s a -> s{_ccfcsTags = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the application.
ccfcsApplicationId :: Lens' CreateCloudFormationChangeSet Text
ccfcsApplicationId = lens _ccfcsApplicationId (\ s a -> s{_ccfcsApplicationId = a})

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
ccfcsStackName :: Lens' CreateCloudFormationChangeSet Text
ccfcsStackName = lens _ccfcsStackName (\ s a -> s{_ccfcsStackName = a})

instance AWSRequest CreateCloudFormationChangeSet
         where
        type Rs CreateCloudFormationChangeSet =
             CreateCloudFormationChangeSetResponse
        request = postJSON serverlessApplicationRepository
        response
          = receiveJSON
              (\ s h x ->
                 CreateCloudFormationChangeSetResponse' <$>
                   (x .?> "semanticVersion") <*> (x .?> "changeSetId")
                     <*> (x .?> "applicationId")
                     <*> (x .?> "stackId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateCloudFormationChangeSet where

instance NFData CreateCloudFormationChangeSet where

instance ToHeaders CreateCloudFormationChangeSet
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCloudFormationChangeSet where
        toJSON CreateCloudFormationChangeSet'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _ccfcsClientToken,
                  ("templateId" .=) <$> _ccfcsTemplateId,
                  ("semanticVersion" .=) <$> _ccfcsSemanticVersion,
                  ("notificationArns" .=) <$> _ccfcsNotificationARNs,
                  ("changeSetName" .=) <$> _ccfcsChangeSetName,
                  ("description" .=) <$> _ccfcsDescription,
                  ("capabilities" .=) <$> _ccfcsCapabilities,
                  ("parameterOverrides" .=) <$>
                    _ccfcsParameterOverrides,
                  ("rollbackConfiguration" .=) <$>
                    _ccfcsRollbackConfiguration,
                  ("resourceTypes" .=) <$> _ccfcsResourceTypes,
                  ("tags" .=) <$> _ccfcsTags,
                  Just ("stackName" .= _ccfcsStackName)])

instance ToPath CreateCloudFormationChangeSet where
        toPath CreateCloudFormationChangeSet'{..}
          = mconcat
              ["/applications/", toBS _ccfcsApplicationId,
               "/changesets"]

instance ToQuery CreateCloudFormationChangeSet where
        toQuery = const mempty

-- | /See:/ 'createCloudFormationChangeSetResponse' smart constructor.
data CreateCloudFormationChangeSetResponse = CreateCloudFormationChangeSetResponse'
  { _ccfcsrsSemanticVersion :: !(Maybe Text)
  , _ccfcsrsChangeSetId     :: !(Maybe Text)
  , _ccfcsrsApplicationId   :: !(Maybe Text)
  , _ccfcsrsStackId         :: !(Maybe Text)
  , _ccfcsrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCloudFormationChangeSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfcsrsSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
--
-- * 'ccfcsrsChangeSetId' - The Amazon Resource Name (ARN) of the change set. Length constraints: Minimum length of 1. Pattern: ARN:[-a-zA-Z0-9:/]*
--
-- * 'ccfcsrsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'ccfcsrsStackId' - The unique ID of the stack.
--
-- * 'ccfcsrsResponseStatus' - -- | The response status code.
createCloudFormationChangeSetResponse
    :: Int -- ^ 'ccfcsrsResponseStatus'
    -> CreateCloudFormationChangeSetResponse
createCloudFormationChangeSetResponse pResponseStatus_ =
  CreateCloudFormationChangeSetResponse'
    { _ccfcsrsSemanticVersion = Nothing
    , _ccfcsrsChangeSetId = Nothing
    , _ccfcsrsApplicationId = Nothing
    , _ccfcsrsStackId = Nothing
    , _ccfcsrsResponseStatus = pResponseStatus_
    }


-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
ccfcsrsSemanticVersion :: Lens' CreateCloudFormationChangeSetResponse (Maybe Text)
ccfcsrsSemanticVersion = lens _ccfcsrsSemanticVersion (\ s a -> s{_ccfcsrsSemanticVersion = a})

-- | The Amazon Resource Name (ARN) of the change set. Length constraints: Minimum length of 1. Pattern: ARN:[-a-zA-Z0-9:/]*
ccfcsrsChangeSetId :: Lens' CreateCloudFormationChangeSetResponse (Maybe Text)
ccfcsrsChangeSetId = lens _ccfcsrsChangeSetId (\ s a -> s{_ccfcsrsChangeSetId = a})

-- | The application Amazon Resource Name (ARN).
ccfcsrsApplicationId :: Lens' CreateCloudFormationChangeSetResponse (Maybe Text)
ccfcsrsApplicationId = lens _ccfcsrsApplicationId (\ s a -> s{_ccfcsrsApplicationId = a})

-- | The unique ID of the stack.
ccfcsrsStackId :: Lens' CreateCloudFormationChangeSetResponse (Maybe Text)
ccfcsrsStackId = lens _ccfcsrsStackId (\ s a -> s{_ccfcsrsStackId = a})

-- | -- | The response status code.
ccfcsrsResponseStatus :: Lens' CreateCloudFormationChangeSetResponse Int
ccfcsrsResponseStatus = lens _ccfcsrsResponseStatus (\ s a -> s{_ccfcsrsResponseStatus = a})

instance NFData CreateCloudFormationChangeSetResponse
         where
