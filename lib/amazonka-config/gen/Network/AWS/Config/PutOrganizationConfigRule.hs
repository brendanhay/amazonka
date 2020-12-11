{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- Only a master account and a delegated administrator can create or update an organization config rule. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
-- This API enables organization service access through the @EnableAWSServiceAccess@ action and creates a service linked role @AWSServiceRoleForConfigMultiAccountSetup@ in the master or delegated administrator account of your organization. The service linked role is created only when the role does not exist in the caller account. AWS Config verifies the existence of role with @GetRole@ action.
-- To use this API with delegated administrator, register a delegated administrator by calling AWS Organization @register-delegated-administrator@ for @config-multiaccountsetup.amazonaws.com@ .
-- You can use this action to create both custom AWS Config rules and AWS managed Config rules. If you are adding a new custom AWS Config rule, you must first create AWS Lambda function in the master account or a delegated administrator that the rule invokes to evaluate your resources. When you use the @PutOrganizationConfigRule@ action to add the rule to AWS Config, you must specify the Amazon Resource Name (ARN) that AWS Lambda assigns to the function. If you are adding an AWS managed Config rule, specify the rule's identifier for the @RuleIdentifier@ key.
-- The maximum number of organization config rules that AWS Config supports is 150 and 3 delegated administrator per organization.
module Network.AWS.Config.PutOrganizationConfigRule
  ( -- * Creating a request
    PutOrganizationConfigRule (..),
    mkPutOrganizationConfigRule,

    -- ** Request lenses
    pocrOrganizationManagedRuleMetadata,
    pocrExcludedAccounts,
    pocrOrganizationCustomRuleMetadata,
    pocrOrganizationConfigRuleName,

    -- * Destructuring the response
    PutOrganizationConfigRuleResponse (..),
    mkPutOrganizationConfigRuleResponse,

    -- ** Response lenses
    pocrrsOrganizationConfigRuleARN,
    pocrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutOrganizationConfigRule' smart constructor.
data PutOrganizationConfigRule = PutOrganizationConfigRule'
  { organizationManagedRuleMetadata ::
      Lude.Maybe
        OrganizationManagedRuleMetadata,
    excludedAccounts ::
      Lude.Maybe [Lude.Text],
    organizationCustomRuleMetadata ::
      Lude.Maybe
        OrganizationCustomRuleMetadata,
    organizationConfigRuleName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutOrganizationConfigRule' with the minimum fields required to make a request.
--
-- * 'excludedAccounts' - A comma-separated list of accounts that you want to exclude from an organization config rule.
-- * 'organizationConfigRuleName' - The name that you assign to an organization config rule.
-- * 'organizationCustomRuleMetadata' - An @OrganizationCustomRuleMetadata@ object.
-- * 'organizationManagedRuleMetadata' - An @OrganizationManagedRuleMetadata@ object.
mkPutOrganizationConfigRule ::
  -- | 'organizationConfigRuleName'
  Lude.Text ->
  PutOrganizationConfigRule
mkPutOrganizationConfigRule pOrganizationConfigRuleName_ =
  PutOrganizationConfigRule'
    { organizationManagedRuleMetadata =
        Lude.Nothing,
      excludedAccounts = Lude.Nothing,
      organizationCustomRuleMetadata = Lude.Nothing,
      organizationConfigRuleName = pOrganizationConfigRuleName_
    }

-- | An @OrganizationManagedRuleMetadata@ object.
--
-- /Note:/ Consider using 'organizationManagedRuleMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocrOrganizationManagedRuleMetadata :: Lens.Lens' PutOrganizationConfigRule (Lude.Maybe OrganizationManagedRuleMetadata)
pocrOrganizationManagedRuleMetadata = Lens.lens (organizationManagedRuleMetadata :: PutOrganizationConfigRule -> Lude.Maybe OrganizationManagedRuleMetadata) (\s a -> s {organizationManagedRuleMetadata = a} :: PutOrganizationConfigRule)
{-# DEPRECATED pocrOrganizationManagedRuleMetadata "Use generic-lens or generic-optics with 'organizationManagedRuleMetadata' instead." #-}

-- | A comma-separated list of accounts that you want to exclude from an organization config rule.
--
-- /Note:/ Consider using 'excludedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocrExcludedAccounts :: Lens.Lens' PutOrganizationConfigRule (Lude.Maybe [Lude.Text])
pocrExcludedAccounts = Lens.lens (excludedAccounts :: PutOrganizationConfigRule -> Lude.Maybe [Lude.Text]) (\s a -> s {excludedAccounts = a} :: PutOrganizationConfigRule)
{-# DEPRECATED pocrExcludedAccounts "Use generic-lens or generic-optics with 'excludedAccounts' instead." #-}

-- | An @OrganizationCustomRuleMetadata@ object.
--
-- /Note:/ Consider using 'organizationCustomRuleMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocrOrganizationCustomRuleMetadata :: Lens.Lens' PutOrganizationConfigRule (Lude.Maybe OrganizationCustomRuleMetadata)
pocrOrganizationCustomRuleMetadata = Lens.lens (organizationCustomRuleMetadata :: PutOrganizationConfigRule -> Lude.Maybe OrganizationCustomRuleMetadata) (\s a -> s {organizationCustomRuleMetadata = a} :: PutOrganizationConfigRule)
{-# DEPRECATED pocrOrganizationCustomRuleMetadata "Use generic-lens or generic-optics with 'organizationCustomRuleMetadata' instead." #-}

-- | The name that you assign to an organization config rule.
--
-- /Note:/ Consider using 'organizationConfigRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocrOrganizationConfigRuleName :: Lens.Lens' PutOrganizationConfigRule Lude.Text
pocrOrganizationConfigRuleName = Lens.lens (organizationConfigRuleName :: PutOrganizationConfigRule -> Lude.Text) (\s a -> s {organizationConfigRuleName = a} :: PutOrganizationConfigRule)
{-# DEPRECATED pocrOrganizationConfigRuleName "Use generic-lens or generic-optics with 'organizationConfigRuleName' instead." #-}

instance Lude.AWSRequest PutOrganizationConfigRule where
  type
    Rs PutOrganizationConfigRule =
      PutOrganizationConfigRuleResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutOrganizationConfigRuleResponse'
            Lude.<$> (x Lude..?> "OrganizationConfigRuleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutOrganizationConfigRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.PutOrganizationConfigRule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutOrganizationConfigRule where
  toJSON PutOrganizationConfigRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OrganizationManagedRuleMetadata" Lude..=)
              Lude.<$> organizationManagedRuleMetadata,
            ("ExcludedAccounts" Lude..=) Lude.<$> excludedAccounts,
            ("OrganizationCustomRuleMetadata" Lude..=)
              Lude.<$> organizationCustomRuleMetadata,
            Lude.Just
              ("OrganizationConfigRuleName" Lude..= organizationConfigRuleName)
          ]
      )

instance Lude.ToPath PutOrganizationConfigRule where
  toPath = Lude.const "/"

instance Lude.ToQuery PutOrganizationConfigRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutOrganizationConfigRuleResponse' smart constructor.
data PutOrganizationConfigRuleResponse = PutOrganizationConfigRuleResponse'
  { organizationConfigRuleARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutOrganizationConfigRuleResponse' with the minimum fields required to make a request.
--
-- * 'organizationConfigRuleARN' - The Amazon Resource Name (ARN) of an organization config rule.
-- * 'responseStatus' - The response status code.
mkPutOrganizationConfigRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutOrganizationConfigRuleResponse
mkPutOrganizationConfigRuleResponse pResponseStatus_ =
  PutOrganizationConfigRuleResponse'
    { organizationConfigRuleARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of an organization config rule.
--
-- /Note:/ Consider using 'organizationConfigRuleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocrrsOrganizationConfigRuleARN :: Lens.Lens' PutOrganizationConfigRuleResponse (Lude.Maybe Lude.Text)
pocrrsOrganizationConfigRuleARN = Lens.lens (organizationConfigRuleARN :: PutOrganizationConfigRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {organizationConfigRuleARN = a} :: PutOrganizationConfigRuleResponse)
{-# DEPRECATED pocrrsOrganizationConfigRuleARN "Use generic-lens or generic-optics with 'organizationConfigRuleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocrrsResponseStatus :: Lens.Lens' PutOrganizationConfigRuleResponse Lude.Int
pocrrsResponseStatus = Lens.lens (responseStatus :: PutOrganizationConfigRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutOrganizationConfigRuleResponse)
{-# DEPRECATED pocrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
