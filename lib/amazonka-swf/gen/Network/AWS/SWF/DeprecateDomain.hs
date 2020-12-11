{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DeprecateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified domain. After a domain has been deprecated it cannot be used to create new workflow executions or register new types. However, you can still use visibility actions on this domain. Deprecating a domain also deprecates all activity and workflow types registered in the domain. Executions that were started before the domain was deprecated continues to run.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.DeprecateDomain
  ( -- * Creating a request
    DeprecateDomain (..),
    mkDeprecateDomain,

    -- ** Request lenses
    dName,

    -- * Destructuring the response
    DeprecateDomainResponse (..),
    mkDeprecateDomainResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkDeprecateDomain' smart constructor.
newtype DeprecateDomain = DeprecateDomain' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeprecateDomain' with the minimum fields required to make a request.
--
-- * 'name' - The name of the domain to deprecate.
mkDeprecateDomain ::
  -- | 'name'
  Lude.Text ->
  DeprecateDomain
mkDeprecateDomain pName_ = DeprecateDomain' {name = pName_}

-- | The name of the domain to deprecate.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DeprecateDomain Lude.Text
dName = Lens.lens (name :: DeprecateDomain -> Lude.Text) (\s a -> s {name = a} :: DeprecateDomain)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeprecateDomain where
  type Rs DeprecateDomain = DeprecateDomainResponse
  request = Req.postJSON swfService
  response = Res.receiveNull DeprecateDomainResponse'

instance Lude.ToHeaders DeprecateDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.DeprecateDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeprecateDomain where
  toJSON DeprecateDomain' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])

instance Lude.ToPath DeprecateDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DeprecateDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeprecateDomainResponse' smart constructor.
data DeprecateDomainResponse = DeprecateDomainResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeprecateDomainResponse' with the minimum fields required to make a request.
mkDeprecateDomainResponse ::
  DeprecateDomainResponse
mkDeprecateDomainResponse = DeprecateDomainResponse'
