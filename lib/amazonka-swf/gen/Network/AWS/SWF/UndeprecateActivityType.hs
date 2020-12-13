{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.UndeprecateActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated /activity type/ . After an activity type has been undeprecated, you can create new tasks of that activity type.
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
module Network.AWS.SWF.UndeprecateActivityType
  ( -- * Creating a request
    UndeprecateActivityType (..),
    mkUndeprecateActivityType,

    -- ** Request lenses
    uatActivityType,
    uatDomain,

    -- * Destructuring the response
    UndeprecateActivityTypeResponse (..),
    mkUndeprecateActivityTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkUndeprecateActivityType' smart constructor.
data UndeprecateActivityType = UndeprecateActivityType'
  { -- | The activity type to undeprecate.
    activityType :: ActivityType,
    -- | The name of the domain of the deprecated activity type.
    domain :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UndeprecateActivityType' with the minimum fields required to make a request.
--
-- * 'activityType' - The activity type to undeprecate.
-- * 'domain' - The name of the domain of the deprecated activity type.
mkUndeprecateActivityType ::
  -- | 'activityType'
  ActivityType ->
  -- | 'domain'
  Lude.Text ->
  UndeprecateActivityType
mkUndeprecateActivityType pActivityType_ pDomain_ =
  UndeprecateActivityType'
    { activityType = pActivityType_,
      domain = pDomain_
    }

-- | The activity type to undeprecate.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatActivityType :: Lens.Lens' UndeprecateActivityType ActivityType
uatActivityType = Lens.lens (activityType :: UndeprecateActivityType -> ActivityType) (\s a -> s {activityType = a} :: UndeprecateActivityType)
{-# DEPRECATED uatActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The name of the domain of the deprecated activity type.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatDomain :: Lens.Lens' UndeprecateActivityType Lude.Text
uatDomain = Lens.lens (domain :: UndeprecateActivityType -> Lude.Text) (\s a -> s {domain = a} :: UndeprecateActivityType)
{-# DEPRECATED uatDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Lude.AWSRequest UndeprecateActivityType where
  type Rs UndeprecateActivityType = UndeprecateActivityTypeResponse
  request = Req.postJSON swfService
  response = Res.receiveNull UndeprecateActivityTypeResponse'

instance Lude.ToHeaders UndeprecateActivityType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.UndeprecateActivityType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UndeprecateActivityType where
  toJSON UndeprecateActivityType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("activityType" Lude..= activityType),
            Lude.Just ("domain" Lude..= domain)
          ]
      )

instance Lude.ToPath UndeprecateActivityType where
  toPath = Lude.const "/"

instance Lude.ToQuery UndeprecateActivityType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUndeprecateActivityTypeResponse' smart constructor.
data UndeprecateActivityTypeResponse = UndeprecateActivityTypeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UndeprecateActivityTypeResponse' with the minimum fields required to make a request.
mkUndeprecateActivityTypeResponse ::
  UndeprecateActivityTypeResponse
mkUndeprecateActivityTypeResponse =
  UndeprecateActivityTypeResponse'
