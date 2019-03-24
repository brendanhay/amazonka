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
-- Updates the stack set, and associated stack instances in the specified accounts and regions.
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
    , ussAccounts
    , ussRegions
    , ussParameters
    , ussOperationPreferences
    , ussOperationId
    , ussTemplateBody
    , ussTemplateURL
    , ussDescription
    , ussCapabilities
    , ussTags
    , ussExecutionRoleName
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
  , _ussAccounts              :: !(Maybe [Text])
  , _ussRegions               :: !(Maybe [Text])
  , _ussParameters            :: !(Maybe [Parameter])
  , _ussOperationPreferences  :: !(Maybe StackSetOperationPreferences)
  , _ussOperationId           :: !(Maybe Text)
  , _ussTemplateBody          :: !(Maybe Text)
  , _ussTemplateURL           :: !(Maybe Text)
  , _ussDescription           :: !(Maybe Text)
  , _ussCapabilities          :: !(Maybe [Capability])
  , _ussTags                  :: !(Maybe [Tag])
  , _ussExecutionRoleName     :: !(Maybe Text)
  , _ussStackSetName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStackSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussAdministrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role to use to update this stack set. Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ . If you specify a customized administrator role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized administrator role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
--
-- * 'ussUsePreviousTemplate' - Use the existing template that's associated with the stack set that you're updating. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@
