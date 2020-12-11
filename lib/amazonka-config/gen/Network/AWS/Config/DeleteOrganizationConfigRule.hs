{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteOrganizationConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified organization config rule and all of its evaluation results from all member accounts in that organization.
--
-- Only a master account and a delegated administrator account can delete an organization config rule. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
-- AWS Config sets the state of a rule to DELETE_IN_PROGRESS until the deletion is complete. You cannot update a rule while it is in this state.
module Network.AWS.Config.DeleteOrganizationConfigRule
  ( -- * Creating a request
    DeleteOrganizationConfigRule (..),
    mkDeleteOrganizationConfigRule,

    -- ** Request lenses
    docrOrganizationConfigRuleName,

    -- * Destructuring the response
    DeleteOrganizationConfigRuleResponse (..),
    mkDeleteOrganizationConfigRuleResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteOrganizationConfigRule' smart constructor.
newtype DeleteOrganizationConfigRule = DeleteOrganizationConfigRule'
  { organizationConfigRuleName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganizationConfigRule' with the minimum fields required to make a request.
--
-- * 'organizationConfigRuleName' - The name of organization config rule that you want to delete.
mkDeleteOrganizationConfigRule ::
  -- | 'organizationConfigRuleName'
  Lude.Text ->
  DeleteOrganizationConfigRule
mkDeleteOrganizationConfigRule pOrganizationConfigRuleName_ =
  DeleteOrganizationConfigRule'
    { organizationConfigRuleName =
        pOrganizationConfigRuleName_
    }

-- | The name of organization config rule that you want to delete.
--
-- /Note:/ Consider using 'organizationConfigRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrOrganizationConfigRuleName :: Lens.Lens' DeleteOrganizationConfigRule Lude.Text
docrOrganizationConfigRuleName = Lens.lens (organizationConfigRuleName :: DeleteOrganizationConfigRule -> Lude.Text) (\s a -> s {organizationConfigRuleName = a} :: DeleteOrganizationConfigRule)
{-# DEPRECATED docrOrganizationConfigRuleName "Use generic-lens or generic-optics with 'organizationConfigRuleName' instead." #-}

instance Lude.AWSRequest DeleteOrganizationConfigRule where
  type
    Rs DeleteOrganizationConfigRule =
      DeleteOrganizationConfigRuleResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeleteOrganizationConfigRuleResponse'

instance Lude.ToHeaders DeleteOrganizationConfigRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DeleteOrganizationConfigRule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteOrganizationConfigRule where
  toJSON DeleteOrganizationConfigRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("OrganizationConfigRuleName" Lude..= organizationConfigRuleName)
          ]
      )

instance Lude.ToPath DeleteOrganizationConfigRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteOrganizationConfigRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteOrganizationConfigRuleResponse' smart constructor.
data DeleteOrganizationConfigRuleResponse = DeleteOrganizationConfigRuleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganizationConfigRuleResponse' with the minimum fields required to make a request.
mkDeleteOrganizationConfigRuleResponse ::
  DeleteOrganizationConfigRuleResponse
mkDeleteOrganizationConfigRuleResponse =
  DeleteOrganizationConfigRuleResponse'
