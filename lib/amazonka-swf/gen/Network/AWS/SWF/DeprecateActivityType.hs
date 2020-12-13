{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DeprecateActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /activity type/ . After an activity type has been deprecated, you cannot create new tasks of that activity type. Tasks of this type that were scheduled before the type was deprecated continue to run.
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
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @activityType.name@ : String constraint. The key is @swf:activityType.name@ .
--
--
--     * @activityType.version@ : String constraint. The key is @swf:activityType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.DeprecateActivityType
  ( -- * Creating a request
    DeprecateActivityType (..),
    mkDeprecateActivityType,

    -- ** Request lenses
    datfActivityType,
    datfDomain,

    -- * Destructuring the response
    DeprecateActivityTypeResponse (..),
    mkDeprecateActivityTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkDeprecateActivityType' smart constructor.
data DeprecateActivityType = DeprecateActivityType'
  { -- | The activity type to deprecate.
    activityType :: ActivityType,
    -- | The name of the domain in which the activity type is registered.
    domain :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeprecateActivityType' with the minimum fields required to make a request.
--
-- * 'activityType' - The activity type to deprecate.
-- * 'domain' - The name of the domain in which the activity type is registered.
mkDeprecateActivityType ::
  -- | 'activityType'
  ActivityType ->
  -- | 'domain'
  Lude.Text ->
  DeprecateActivityType
mkDeprecateActivityType pActivityType_ pDomain_ =
  DeprecateActivityType'
    { activityType = pActivityType_,
      domain = pDomain_
    }

-- | The activity type to deprecate.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datfActivityType :: Lens.Lens' DeprecateActivityType ActivityType
datfActivityType = Lens.lens (activityType :: DeprecateActivityType -> ActivityType) (\s a -> s {activityType = a} :: DeprecateActivityType)
{-# DEPRECATED datfActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The name of the domain in which the activity type is registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datfDomain :: Lens.Lens' DeprecateActivityType Lude.Text
datfDomain = Lens.lens (domain :: DeprecateActivityType -> Lude.Text) (\s a -> s {domain = a} :: DeprecateActivityType)
{-# DEPRECATED datfDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Lude.AWSRequest DeprecateActivityType where
  type Rs DeprecateActivityType = DeprecateActivityTypeResponse
  request = Req.postJSON swfService
  response = Res.receiveNull DeprecateActivityTypeResponse'

instance Lude.ToHeaders DeprecateActivityType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.DeprecateActivityType" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeprecateActivityType where
  toJSON DeprecateActivityType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("activityType" Lude..= activityType),
            Lude.Just ("domain" Lude..= domain)
          ]
      )

instance Lude.ToPath DeprecateActivityType where
  toPath = Lude.const "/"

instance Lude.ToQuery DeprecateActivityType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeprecateActivityTypeResponse' smart constructor.
data DeprecateActivityTypeResponse = DeprecateActivityTypeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeprecateActivityTypeResponse' with the minimum fields required to make a request.
mkDeprecateActivityTypeResponse ::
  DeprecateActivityTypeResponse
mkDeprecateActivityTypeResponse = DeprecateActivityTypeResponse'
