{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.UndeprecateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated domain. After a domain has been undeprecated it can be used to create new workflow executions or register new types.
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
module Network.AWS.SWF.UndeprecateDomain
  ( -- * Creating a request
    UndeprecateDomain (..),
    mkUndeprecateDomain,

    -- ** Request lenses
    udName,

    -- * Destructuring the response
    UndeprecateDomainResponse (..),
    mkUndeprecateDomainResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkUndeprecateDomain' smart constructor.
newtype UndeprecateDomain = UndeprecateDomain' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UndeprecateDomain' with the minimum fields required to make a request.
--
-- * 'name' - The name of the domain of the deprecated workflow type.
mkUndeprecateDomain ::
  -- | 'name'
  Lude.Text ->
  UndeprecateDomain
mkUndeprecateDomain pName_ = UndeprecateDomain' {name = pName_}

-- | The name of the domain of the deprecated workflow type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UndeprecateDomain Lude.Text
udName = Lens.lens (name :: UndeprecateDomain -> Lude.Text) (\s a -> s {name = a} :: UndeprecateDomain)
{-# DEPRECATED udName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UndeprecateDomain where
  type Rs UndeprecateDomain = UndeprecateDomainResponse
  request = Req.postJSON swfService
  response = Res.receiveNull UndeprecateDomainResponse'

instance Lude.ToHeaders UndeprecateDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.UndeprecateDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UndeprecateDomain where
  toJSON UndeprecateDomain' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])

instance Lude.ToPath UndeprecateDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery UndeprecateDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUndeprecateDomainResponse' smart constructor.
data UndeprecateDomainResponse = UndeprecateDomainResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UndeprecateDomainResponse' with the minimum fields required to make a request.
mkUndeprecateDomainResponse ::
  UndeprecateDomainResponse
mkUndeprecateDomainResponse = UndeprecateDomainResponse'
