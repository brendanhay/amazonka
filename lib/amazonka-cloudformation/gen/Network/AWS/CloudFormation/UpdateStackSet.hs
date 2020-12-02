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
-- Module      : Network.AWS.CloudFormation.UpdateStackSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the stack set and /all/ associated stack instances.
--
--
-- Even if the stack set operation created by updating the stack set fails (completely or partially, below or above a specified failure tolerance), the stack set is updated with your changes. Subsequent 'CreateStackInstances' calls on the specified stack set use the updated stack set.
--
module Network.AWS.CloudFormation.UpdateStackSet
    (
    -- * Creating a Request
      updateStackSet
    , UpdateStackSet
    -- * Request Lenses
    , ussAdministrationRoleARN
    , ussUsePreviousTemplate
    , ussParameters
    , ussOperationPreferences
    , ussOperationId
    , ussTemplateBody
    , ussTemplateURL
    , ussDescription
    , ussCapabilities
    , ussTags
    , ussStackSetName

    -- * Destructuring the Response
    , updateStackSetResponse
    , UpdateStackSetResponse
    -- * Response Lenses
    , ussrsOperationId
    , ussrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStackSet' smart constructor.
data UpdateStackSet = UpdateStackSet'
  { _ussAdministrationRoleARN :: !(Maybe Text)
  , _ussUsePreviousTemplate   :: !(Maybe Bool)
  , _ussParameters            :: !(Maybe [Parameter])
  , _ussOperationPreferences  :: !(Maybe StackSetOperationPreferences)
  , _ussOperationId           :: !(Maybe Text)
  , _ussTemplateBody          :: !(Maybe Text)
  , _ussTemplateURL           :: !(Maybe Text)
  , _ussDescription           :: !(Maybe Text)
  , _ussCapabilities          :: !(Maybe [Capability])
  , _ussTags                  :: !(Maybe [Tag])
  , _ussStackSetName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStackSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussAdministrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role to use to update this stack set. Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ . If you specify a customized administrator role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized administrator role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
--
-- * 'ussUsePreviousTemplate' - Use the existing template that's associated with the stack set that you're updating. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- * 'ussParameters' - A list of input parameters for the stack set template.
--
-- * 'ussOperationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
--
-- * 'ussOperationId' - The unique ID for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, AWS CloudFormation generates one automatically. Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- * 'ussTemplateBody' - The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- * 'ussTemplateURL' - The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- * 'ussDescription' - A brief description of updates that you are making.
--
-- * 'ussCapabilities' - A list of values that you must specify before AWS CloudFormation can create certain stack sets. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For those stack sets, you must explicitly acknowledge their capabilities by specifying this parameter. The only valid values are CAPABILITY_IAM and CAPABILITY_NAMED_IAM. The following resources require you to specify this parameter:      * AWS::IAM::AccessKey     * AWS::IAM::Group     * AWS::IAM::InstanceProfile     * AWS::IAM::Policy     * AWS::IAM::Role     * AWS::IAM::User     * AWS::IAM::UserToGroupAddition If your stack template contains these resources, we recommend that you review all permissions that are associated with them and edit their permissions if necessary. If you have IAM resources, you can specify either capability. If you have IAM resources with custom names, you must specify CAPABILITY_NAMED_IAM. If you don't specify this parameter, this action returns an @InsufficientCapabilities@ error. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
--
-- * 'ussTags' - The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. You can specify a maximum number of 50 tags. If you specify tags for this parameter, those tags replace any list of tags that are currently associated with this stack set. This means:     * If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags.      * If you specify /any/ tags using this parameter, you must specify /all/ the tags that you want associated with this stack set, even tags you've specifed before (for example, when creating the stack set or during a previous update of the stack set.). Any tags that you don't include in the updated list of tags are removed from the stack set, and therefore from the stacks and resources as well.      * If you specify an empty value, AWS CloudFormation removes all currently associated tags. If you specify new tags as part of an @UpdateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you omit tags that are currently associated with the stack set from the list of tags you specify, AWS CloudFormation assumes that you want to remove those tags from the stack set, and checks to see if you have permission to untag resources. If you don't have the necessary permission(s), the entire @UpdateStackSet@ action fails with an @access denied@ error, and the stack set is not updated.
--
-- * 'ussStackSetName' - The name or unique ID of the stack set that you want to update.
updateStackSet
    :: Text -- ^ 'ussStackSetName'
    -> UpdateStackSet
updateStackSet pStackSetName_ =
  UpdateStackSet'
    { _ussAdministrationRoleARN = Nothing
    , _ussUsePreviousTemplate = Nothing
    , _ussParameters = Nothing
    , _ussOperationPreferences = Nothing
    , _ussOperationId = Nothing
    , _ussTemplateBody = Nothing
    , _ussTemplateURL = Nothing
    , _ussDescription = Nothing
    , _ussCapabilities = Nothing
    , _ussTags = Nothing
    , _ussStackSetName = pStackSetName_
    }


-- | The Amazon Resource Number (ARN) of the IAM role to use to update this stack set. Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ . If you specify a customized administrator role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized administrator role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
ussAdministrationRoleARN :: Lens' UpdateStackSet (Maybe Text)
ussAdministrationRoleARN = lens _ussAdministrationRoleARN (\ s a -> s{_ussAdministrationRoleARN = a})

-- | Use the existing template that's associated with the stack set that you're updating. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
ussUsePreviousTemplate :: Lens' UpdateStackSet (Maybe Bool)
ussUsePreviousTemplate = lens _ussUsePreviousTemplate (\ s a -> s{_ussUsePreviousTemplate = a})

-- | A list of input parameters for the stack set template.
ussParameters :: Lens' UpdateStackSet [Parameter]
ussParameters = lens _ussParameters (\ s a -> s{_ussParameters = a}) . _Default . _Coerce

-- | Preferences for how AWS CloudFormation performs this stack set operation.
ussOperationPreferences :: Lens' UpdateStackSet (Maybe StackSetOperationPreferences)
ussOperationPreferences = lens _ussOperationPreferences (\ s a -> s{_ussOperationPreferences = a})

-- | The unique ID for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, AWS CloudFormation generates one automatically. Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
ussOperationId :: Lens' UpdateStackSet (Maybe Text)
ussOperationId = lens _ussOperationId (\ s a -> s{_ussOperationId = a})

-- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
ussTemplateBody :: Lens' UpdateStackSet (Maybe Text)
ussTemplateBody = lens _ussTemplateBody (\ s a -> s{_ussTemplateBody = a})

-- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
ussTemplateURL :: Lens' UpdateStackSet (Maybe Text)
ussTemplateURL = lens _ussTemplateURL (\ s a -> s{_ussTemplateURL = a})

-- | A brief description of updates that you are making.
ussDescription :: Lens' UpdateStackSet (Maybe Text)
ussDescription = lens _ussDescription (\ s a -> s{_ussDescription = a})

-- | A list of values that you must specify before AWS CloudFormation can create certain stack sets. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For those stack sets, you must explicitly acknowledge their capabilities by specifying this parameter. The only valid values are CAPABILITY_IAM and CAPABILITY_NAMED_IAM. The following resources require you to specify this parameter:      * AWS::IAM::AccessKey     * AWS::IAM::Group     * AWS::IAM::InstanceProfile     * AWS::IAM::Policy     * AWS::IAM::Role     * AWS::IAM::User     * AWS::IAM::UserToGroupAddition If your stack template contains these resources, we recommend that you review all permissions that are associated with them and edit their permissions if necessary. If you have IAM resources, you can specify either capability. If you have IAM resources with custom names, you must specify CAPABILITY_NAMED_IAM. If you don't specify this parameter, this action returns an @InsufficientCapabilities@ error. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
ussCapabilities :: Lens' UpdateStackSet [Capability]
ussCapabilities = lens _ussCapabilities (\ s a -> s{_ussCapabilities = a}) . _Default . _Coerce

-- | The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. You can specify a maximum number of 50 tags. If you specify tags for this parameter, those tags replace any list of tags that are currently associated with this stack set. This means:     * If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags.      * If you specify /any/ tags using this parameter, you must specify /all/ the tags that you want associated with this stack set, even tags you've specifed before (for example, when creating the stack set or during a previous update of the stack set.). Any tags that you don't include in the updated list of tags are removed from the stack set, and therefore from the stacks and resources as well.      * If you specify an empty value, AWS CloudFormation removes all currently associated tags. If you specify new tags as part of an @UpdateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you omit tags that are currently associated with the stack set from the list of tags you specify, AWS CloudFormation assumes that you want to remove those tags from the stack set, and checks to see if you have permission to untag resources. If you don't have the necessary permission(s), the entire @UpdateStackSet@ action fails with an @access denied@ error, and the stack set is not updated.
ussTags :: Lens' UpdateStackSet [Tag]
ussTags = lens _ussTags (\ s a -> s{_ussTags = a}) . _Default . _Coerce

-- | The name or unique ID of the stack set that you want to update.
ussStackSetName :: Lens' UpdateStackSet Text
ussStackSetName = lens _ussStackSetName (\ s a -> s{_ussStackSetName = a})

instance AWSRequest UpdateStackSet where
        type Rs UpdateStackSet = UpdateStackSetResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "UpdateStackSetResult"
              (\ s h x ->
                 UpdateStackSetResponse' <$>
                   (x .@? "OperationId") <*> (pure (fromEnum s)))

instance Hashable UpdateStackSet where

instance NFData UpdateStackSet where

instance ToHeaders UpdateStackSet where
        toHeaders = const mempty

instance ToPath UpdateStackSet where
        toPath = const "/"

instance ToQuery UpdateStackSet where
        toQuery UpdateStackSet'{..}
          = mconcat
              ["Action" =: ("UpdateStackSet" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "AdministrationRoleARN" =: _ussAdministrationRoleARN,
               "UsePreviousTemplate" =: _ussUsePreviousTemplate,
               "Parameters" =:
                 toQuery (toQueryList "member" <$> _ussParameters),
               "OperationPreferences" =: _ussOperationPreferences,
               "OperationId" =: _ussOperationId,
               "TemplateBody" =: _ussTemplateBody,
               "TemplateURL" =: _ussTemplateURL,
               "Description" =: _ussDescription,
               "Capabilities" =:
                 toQuery (toQueryList "member" <$> _ussCapabilities),
               "Tags" =:
                 toQuery (toQueryList "member" <$> _ussTags),
               "StackSetName" =: _ussStackSetName]

-- | /See:/ 'updateStackSetResponse' smart constructor.
data UpdateStackSetResponse = UpdateStackSetResponse'
  { _ussrsOperationId    :: !(Maybe Text)
  , _ussrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStackSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussrsOperationId' - The unique ID for this stack set operation.
--
-- * 'ussrsResponseStatus' - -- | The response status code.
updateStackSetResponse
    :: Int -- ^ 'ussrsResponseStatus'
    -> UpdateStackSetResponse
updateStackSetResponse pResponseStatus_ =
  UpdateStackSetResponse'
    {_ussrsOperationId = Nothing, _ussrsResponseStatus = pResponseStatus_}


-- | The unique ID for this stack set operation.
ussrsOperationId :: Lens' UpdateStackSetResponse (Maybe Text)
ussrsOperationId = lens _ussrsOperationId (\ s a -> s{_ussrsOperationId = a})

-- | -- | The response status code.
ussrsResponseStatus :: Lens' UpdateStackSetResponse Int
ussrsResponseStatus = lens _ussrsResponseStatus (\ s a -> s{_ussrsResponseStatus = a})

instance NFData UpdateStackSetResponse where
