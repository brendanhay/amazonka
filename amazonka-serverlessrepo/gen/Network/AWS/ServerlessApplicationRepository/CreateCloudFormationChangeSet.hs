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
-- Creates an AWS CloudFormation ChangeSet for the given application.
--
--
module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationChangeSet
    (
    -- * Creating a Request
      createCloudFormationChangeSet
    , CreateCloudFormationChangeSet
    -- * Request Lenses
    , ccfcsSemanticVersion
    , ccfcsParameterOverrides
    , ccfcsStackName
    , ccfcsApplicationId

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
  { _ccfcsSemanticVersion    :: !(Maybe Text)
  , _ccfcsParameterOverrides :: !(Maybe [ParameterValue])
  , _ccfcsStackName          :: !(Maybe Text)
  , _ccfcsApplicationId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCloudFormationChangeSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfcsSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
--
-- * 'ccfcsParameterOverrides' - A list of parameter values for the parameters of the application.
--
-- * 'ccfcsStackName' - The name or the unique ID of the stack for which you are creating a change set. AWS CloudFormation generates  the change set by comparing this stack's information with the information that you submit, such as a modified  template or different parameter input values.  Constraints: Minimum length of 1. Pattern: ([a-zA-Z][-a-zA-Z0-9]*)|(arn:\b(aws|aws-us-gov|aws-cn)\b:[-a-zA-Z0-9:/._+]*)
--
-- * 'ccfcsApplicationId' - The ID of the application to get.
createCloudFormationChangeSet
    :: Text -- ^ 'ccfcsApplicationId'
    -> CreateCloudFormationChangeSet
createCloudFormationChangeSet pApplicationId_ =
  CreateCloudFormationChangeSet'
    { _ccfcsSemanticVersion = Nothing
    , _ccfcsParameterOverrides = Nothing
    , _ccfcsStackName = Nothing
    , _ccfcsApplicationId = pApplicationId_
    }


-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
ccfcsSemanticVersion :: Lens' CreateCloudFormationChangeSet (Maybe Text)
ccfcsSemanticVersion = lens _ccfcsSemanticVersion (\ s a -> s{_ccfcsSemanticVersion = a})

-- | A list of parameter values for the parameters of the application.
ccfcsParameterOverrides :: Lens' CreateCloudFormationChangeSet [ParameterValue]
ccfcsParameterOverrides = lens _ccfcsParameterOverrides (\ s a -> s{_ccfcsParameterOverrides = a}) . _Default . _Coerce

-- | The name or the unique ID of the stack for which you are creating a change set. AWS CloudFormation generates  the change set by comparing this stack's information with the information that you submit, such as a modified  template or different parameter input values.  Constraints: Minimum length of 1. Pattern: ([a-zA-Z][-a-zA-Z0-9]*)|(arn:\b(aws|aws-us-gov|aws-cn)\b:[-a-zA-Z0-9:/._+]*)
ccfcsStackName :: Lens' CreateCloudFormationChangeSet (Maybe Text)
ccfcsStackName = lens _ccfcsStackName (\ s a -> s{_ccfcsStackName = a})

-- | The ID of the application to get.
ccfcsApplicationId :: Lens' CreateCloudFormationChangeSet Text
ccfcsApplicationId = lens _ccfcsApplicationId (\ s a -> s{_ccfcsApplicationId = a})

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
                 [("semanticVersion" .=) <$> _ccfcsSemanticVersion,
                  ("parameterOverrides" .=) <$>
                    _ccfcsParameterOverrides,
                  ("stackName" .=) <$> _ccfcsStackName])

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
-- * 'ccfcsrsChangeSetId' - The ARN of the change set. Length Constraints: Minimum length of 1. Pattern: Amazon Resource Name (ARN):[-a-zA-Z0-9:/]*
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

-- | The ARN of the change set. Length Constraints: Minimum length of 1. Pattern: Amazon Resource Name (ARN):[-a-zA-Z0-9:/]*
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
