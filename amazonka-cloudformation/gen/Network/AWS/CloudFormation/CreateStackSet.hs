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
-- Module      : Network.AWS.CloudFormation.CreateStackSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack set.
--
--
module Network.AWS.CloudFormation.CreateStackSet
    (
    -- * Creating a Request
      createStackSet
    , CreateStackSet
    -- * Request Lenses
    , cssAdministrationRoleARN
    , cssParameters
    , cssTemplateBody
    , cssTemplateURL
    , cssClientRequestToken
    , cssDescription
    , cssCapabilities
    , cssTags
    , cssStackSetName

    -- * Destructuring the Response
    , createStackSetResponse
    , CreateStackSetResponse
    -- * Response Lenses
    , cssrsStackSetId
    , cssrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStackSet' smart constructor.
data CreateStackSet = CreateStackSet'
  { _cssAdministrationRoleARN :: !(Maybe Text)
  , _cssParameters            :: !(Maybe [Parameter])
  , _cssTemplateBody          :: !(Maybe Text)
  , _cssTemplateURL           :: !(Maybe Text)
  , _cssClientRequestToken    :: !(Maybe Text)
  , _cssDescription           :: !(Maybe Text)
  , _cssCapabilities          :: !(Maybe [Capability])
  , _cssTags                  :: !(Maybe [Tag])
  , _cssStackSetName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStackSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssAdministrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role to use to create this stack set.  Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
--
-- * 'cssParameters' - The input parameters for the stack set template.
--
-- * 'cssTemplateBody' - The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
--
-- * 'cssTemplateURL' - The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that's located in an Amazon S3 bucket. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
--
-- * 'cssClientRequestToken' - A unique identifier for this @CreateStackSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another stack set with the same name. You might retry @CreateStackSet@ requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.
--
-- * 'cssDescription' - A description of the stack set. You can use the description to identify the stack set's purpose or other important information.
--
-- * 'cssCapabilities' - A list of values that you must specify before AWS CloudFormation can create certain stack sets. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For those stack sets, you must explicitly acknowledge their capabilities by specifying this parameter. The only valid values are CAPABILITY_IAM and CAPABILITY_NAMED_IAM. The following resources require you to specify this parameter:      * AWS::IAM::AccessKey     * AWS::IAM::Group     * AWS::IAM::InstanceProfile     * AWS::IAM::Policy     * AWS::IAM::Role     * AWS::IAM::User     * AWS::IAM::UserToGroupAddition If your stack template contains these resources, we recommend that you review all permissions that are associated with them and edit their permissions if necessary. If you have IAM resources, you can specify either capability. If you have IAM resources with custom names, you must specify CAPABILITY_NAMED_IAM. If you don't specify this parameter, this action returns an @InsufficientCapabilities@ error. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
--
-- * 'cssTags' - The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. A maximum number of 50 tags can be specified. If you specify tags as part of a @CreateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you don't, the entire @CreateStackSet@ action fails with an @access denied@ error, and the stack set is not created.
--
-- * 'cssStackSetName' - The name to associate with the stack set. The name must be unique in the region where you create your stack set.
createStackSet
    :: Text -- ^ 'cssStackSetName'
    -> CreateStackSet
createStackSet pStackSetName_ =
  CreateStackSet'
    { _cssAdministrationRoleARN = Nothing
    , _cssParameters = Nothing
    , _cssTemplateBody = Nothing
    , _cssTemplateURL = Nothing
    , _cssClientRequestToken = Nothing
    , _cssDescription = Nothing
    , _cssCapabilities = Nothing
    , _cssTags = Nothing
    , _cssStackSetName = pStackSetName_
    }


-- | The Amazon Resource Number (ARN) of the IAM role to use to create this stack set.  Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
cssAdministrationRoleARN :: Lens' CreateStackSet (Maybe Text)
cssAdministrationRoleARN = lens _cssAdministrationRoleARN (\ s a -> s{_cssAdministrationRoleARN = a})

-- | The input parameters for the stack set template.
cssParameters :: Lens' CreateStackSet [Parameter]
cssParameters = lens _cssParameters (\ s a -> s{_cssParameters = a}) . _Default . _Coerce

-- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
cssTemplateBody :: Lens' CreateStackSet (Maybe Text)
cssTemplateBody = lens _cssTemplateBody (\ s a -> s{_cssTemplateBody = a})

-- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that's located in an Amazon S3 bucket. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
cssTemplateURL :: Lens' CreateStackSet (Maybe Text)
cssTemplateURL = lens _cssTemplateURL (\ s a -> s{_cssTemplateURL = a})

-- | A unique identifier for this @CreateStackSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another stack set with the same name. You might retry @CreateStackSet@ requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.
cssClientRequestToken :: Lens' CreateStackSet (Maybe Text)
cssClientRequestToken = lens _cssClientRequestToken (\ s a -> s{_cssClientRequestToken = a})

-- | A description of the stack set. You can use the description to identify the stack set's purpose or other important information.
cssDescription :: Lens' CreateStackSet (Maybe Text)
cssDescription = lens _cssDescription (\ s a -> s{_cssDescription = a})

-- | A list of values that you must specify before AWS CloudFormation can create certain stack sets. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For those stack sets, you must explicitly acknowledge their capabilities by specifying this parameter. The only valid values are CAPABILITY_IAM and CAPABILITY_NAMED_IAM. The following resources require you to specify this parameter:      * AWS::IAM::AccessKey     * AWS::IAM::Group     * AWS::IAM::InstanceProfile     * AWS::IAM::Policy     * AWS::IAM::Role     * AWS::IAM::User     * AWS::IAM::UserToGroupAddition If your stack template contains these resources, we recommend that you review all permissions that are associated with them and edit their permissions if necessary. If you have IAM resources, you can specify either capability. If you have IAM resources with custom names, you must specify CAPABILITY_NAMED_IAM. If you don't specify this parameter, this action returns an @InsufficientCapabilities@ error. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
cssCapabilities :: Lens' CreateStackSet [Capability]
cssCapabilities = lens _cssCapabilities (\ s a -> s{_cssCapabilities = a}) . _Default . _Coerce

-- | The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. A maximum number of 50 tags can be specified. If you specify tags as part of a @CreateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you don't, the entire @CreateStackSet@ action fails with an @access denied@ error, and the stack set is not created.
cssTags :: Lens' CreateStackSet [Tag]
cssTags = lens _cssTags (\ s a -> s{_cssTags = a}) . _Default . _Coerce

-- | The name to associate with the stack set. The name must be unique in the region where you create your stack set.
cssStackSetName :: Lens' CreateStackSet Text
cssStackSetName = lens _cssStackSetName (\ s a -> s{_cssStackSetName = a})

instance AWSRequest CreateStackSet where
        type Rs CreateStackSet = CreateStackSetResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "CreateStackSetResult"
              (\ s h x ->
                 CreateStackSetResponse' <$>
                   (x .@? "StackSetId") <*> (pure (fromEnum s)))

instance Hashable CreateStackSet where

instance NFData CreateStackSet where

instance ToHeaders CreateStackSet where
        toHeaders = const mempty

instance ToPath CreateStackSet where
        toPath = const "/"

instance ToQuery CreateStackSet where
        toQuery CreateStackSet'{..}
          = mconcat
              ["Action" =: ("CreateStackSet" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "AdministrationRoleARN" =: _cssAdministrationRoleARN,
               "Parameters" =:
                 toQuery (toQueryList "member" <$> _cssParameters),
               "TemplateBody" =: _cssTemplateBody,
               "TemplateURL" =: _cssTemplateURL,
               "ClientRequestToken" =: _cssClientRequestToken,
               "Description" =: _cssDescription,
               "Capabilities" =:
                 toQuery (toQueryList "member" <$> _cssCapabilities),
               "Tags" =:
                 toQuery (toQueryList "member" <$> _cssTags),
               "StackSetName" =: _cssStackSetName]

-- | /See:/ 'createStackSetResponse' smart constructor.
data CreateStackSetResponse = CreateStackSetResponse'
  { _cssrsStackSetId     :: !(Maybe Text)
  , _cssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStackSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssrsStackSetId' - The ID of the stack set that you're creating.
--
-- * 'cssrsResponseStatus' - -- | The response status code.
createStackSetResponse
    :: Int -- ^ 'cssrsResponseStatus'
    -> CreateStackSetResponse
createStackSetResponse pResponseStatus_ =
  CreateStackSetResponse'
    {_cssrsStackSetId = Nothing, _cssrsResponseStatus = pResponseStatus_}


-- | The ID of the stack set that you're creating.
cssrsStackSetId :: Lens' CreateStackSetResponse (Maybe Text)
cssrsStackSetId = lens _cssrsStackSetId (\ s a -> s{_cssrsStackSetId = a})

-- | -- | The response status code.
cssrsResponseStatus :: Lens' CreateStackSetResponse Int
cssrsResponseStatus = lens _cssrsResponseStatus (\ s a -> s{_cssrsResponseStatus = a})

instance NFData CreateStackSetResponse where
