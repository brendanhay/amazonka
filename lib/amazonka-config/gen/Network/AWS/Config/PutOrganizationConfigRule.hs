{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutOrganizationConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates organization config rule for your entire organization evaluating whether your AWS resources comply with your desired configurations.
--
--
-- Only a master account and a delegated administrator can create or update an organization config rule. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
--
-- This API enables organization service access through the @EnableAWSServiceAccess@ action and creates a service linked role @AWSServiceRoleForConfigMultiAccountSetup@ in the master or delegated administrator account of your organization. The service linked role is created only when the role does not exist in the caller account. AWS Config verifies the existence of role with @GetRole@ action.
--
-- To use this API with delegated administrator, register a delegated administrator by calling AWS Organization @register-delegated-administrator@ for @config-multiaccountsetup.amazonaws.com@ .
--
-- You can use this action to create both custom AWS Config rules and AWS managed Config rules. If you are adding a new custom AWS Config rule, you must first create AWS Lambda function in the master account or a delegated administrator that the rule invokes to evaluate your resources. When you use the @PutOrganizationConfigRule@ action to add the rule to AWS Config, you must specify the Amazon Resource Name (ARN) that AWS Lambda assigns to the function. If you are adding an AWS managed Config rule, specify the rule's identifier for the @RuleIdentifier@ key.
--
-- The maximum number of organization config rules that AWS Config supports is 150 and 3 delegated administrator per organization.
module Network.AWS.Config.PutOrganizationConfigRule
  ( -- * Creating a Request
    putOrganizationConfigRule,
    PutOrganizationConfigRule,

    -- * Request Lenses
    pocrOrganizationManagedRuleMetadata,
    pocrExcludedAccounts,
    pocrOrganizationCustomRuleMetadata,
    pocrOrganizationConfigRuleName,

    -- * Destructuring the Response
    putOrganizationConfigRuleResponse,
    PutOrganizationConfigRuleResponse,

    -- * Response Lenses
    pocrrsOrganizationConfigRuleARN,
    pocrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putOrganizationConfigRule' smart constructor.
data PutOrganizationConfigRule = PutOrganizationConfigRule'
  { _pocrOrganizationManagedRuleMetadata ::
      !( Maybe
           OrganizationManagedRuleMetadata
       ),
    _pocrExcludedAccounts ::
      !(Maybe [Text]),
    _pocrOrganizationCustomRuleMetadata ::
      !(Maybe OrganizationCustomRuleMetadata),
    _pocrOrganizationConfigRuleName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutOrganizationConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pocrOrganizationManagedRuleMetadata' - An @OrganizationManagedRuleMetadata@ object.
--
-- * 'pocrExcludedAccounts' - A comma-separated list of accounts that you want to exclude from an organization config rule.
--
-- * 'pocrOrganizationCustomRuleMetadata' - An @OrganizationCustomRuleMetadata@ object.
--
-- * 'pocrOrganizationConfigRuleName' - The name that you assign to an organization config rule.
putOrganizationConfigRule ::
  -- | 'pocrOrganizationConfigRuleName'
  Text ->
  PutOrganizationConfigRule
putOrganizationConfigRule pOrganizationConfigRuleName_ =
  PutOrganizationConfigRule'
    { _pocrOrganizationManagedRuleMetadata =
        Nothing,
      _pocrExcludedAccounts = Nothing,
      _pocrOrganizationCustomRuleMetadata = Nothing,
      _pocrOrganizationConfigRuleName = pOrganizationConfigRuleName_
    }

-- | An @OrganizationManagedRuleMetadata@ object.
pocrOrganizationManagedRuleMetadata :: Lens' PutOrganizationConfigRule (Maybe OrganizationManagedRuleMetadata)
pocrOrganizationManagedRuleMetadata = lens _pocrOrganizationManagedRuleMetadata (\s a -> s {_pocrOrganizationManagedRuleMetadata = a})

-- | A comma-separated list of accounts that you want to exclude from an organization config rule.
pocrExcludedAccounts :: Lens' PutOrganizationConfigRule [Text]
pocrExcludedAccounts = lens _pocrExcludedAccounts (\s a -> s {_pocrExcludedAccounts = a}) . _Default . _Coerce

-- | An @OrganizationCustomRuleMetadata@ object.
pocrOrganizationCustomRuleMetadata :: Lens' PutOrganizationConfigRule (Maybe OrganizationCustomRuleMetadata)
pocrOrganizationCustomRuleMetadata = lens _pocrOrganizationCustomRuleMetadata (\s a -> s {_pocrOrganizationCustomRuleMetadata = a})

-- | The name that you assign to an organization config rule.
pocrOrganizationConfigRuleName :: Lens' PutOrganizationConfigRule Text
pocrOrganizationConfigRuleName = lens _pocrOrganizationConfigRuleName (\s a -> s {_pocrOrganizationConfigRuleName = a})

instance AWSRequest PutOrganizationConfigRule where
  type
    Rs PutOrganizationConfigRule =
      PutOrganizationConfigRuleResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          PutOrganizationConfigRuleResponse'
            <$> (x .?> "OrganizationConfigRuleArn") <*> (pure (fromEnum s))
      )

instance Hashable PutOrganizationConfigRule

instance NFData PutOrganizationConfigRule

instance ToHeaders PutOrganizationConfigRule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StarlingDoveService.PutOrganizationConfigRule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutOrganizationConfigRule where
  toJSON PutOrganizationConfigRule' {..} =
    object
      ( catMaybes
          [ ("OrganizationManagedRuleMetadata" .=)
              <$> _pocrOrganizationManagedRuleMetadata,
            ("ExcludedAccounts" .=) <$> _pocrExcludedAccounts,
            ("OrganizationCustomRuleMetadata" .=)
              <$> _pocrOrganizationCustomRuleMetadata,
            Just
              ("OrganizationConfigRuleName" .= _pocrOrganizationConfigRuleName)
          ]
      )

instance ToPath PutOrganizationConfigRule where
  toPath = const "/"

instance ToQuery PutOrganizationConfigRule where
  toQuery = const mempty

-- | /See:/ 'putOrganizationConfigRuleResponse' smart constructor.
data PutOrganizationConfigRuleResponse = PutOrganizationConfigRuleResponse'
  { _pocrrsOrganizationConfigRuleARN ::
      !(Maybe Text),
    _pocrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutOrganizationConfigRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pocrrsOrganizationConfigRuleARN' - The Amazon Resource Name (ARN) of an organization config rule.
--
-- * 'pocrrsResponseStatus' - -- | The response status code.
putOrganizationConfigRuleResponse ::
  -- | 'pocrrsResponseStatus'
  Int ->
  PutOrganizationConfigRuleResponse
putOrganizationConfigRuleResponse pResponseStatus_ =
  PutOrganizationConfigRuleResponse'
    { _pocrrsOrganizationConfigRuleARN =
        Nothing,
      _pocrrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of an organization config rule.
pocrrsOrganizationConfigRuleARN :: Lens' PutOrganizationConfigRuleResponse (Maybe Text)
pocrrsOrganizationConfigRuleARN = lens _pocrrsOrganizationConfigRuleARN (\s a -> s {_pocrrsOrganizationConfigRuleARN = a})

-- | -- | The response status code.
pocrrsResponseStatus :: Lens' PutOrganizationConfigRuleResponse Int
pocrrsResponseStatus = lens _pocrrsResponseStatus (\s a -> s {_pocrrsResponseStatus = a})

instance NFData PutOrganizationConfigRuleResponse
