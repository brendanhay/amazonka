{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists an environment's upcoming and in-progress managed actions.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions
  ( -- * Creating a request
    DescribeEnvironmentManagedActions (..),
    mkDescribeEnvironmentManagedActions,

    -- ** Request lenses
    demaStatus,
    demaEnvironmentName,
    demaEnvironmentId,

    -- * Destructuring the response
    DescribeEnvironmentManagedActionsResponse (..),
    mkDescribeEnvironmentManagedActionsResponse,

    -- ** Response lenses
    demarsManagedActions,
    demarsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to list an environment's upcoming and in-progress managed actions.
--
-- /See:/ 'mkDescribeEnvironmentManagedActions' smart constructor.
data DescribeEnvironmentManagedActions = DescribeEnvironmentManagedActions'
  { status ::
      Lude.Maybe ActionStatus,
    environmentName ::
      Lude.Maybe Lude.Text,
    environmentId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentManagedActions' with the minimum fields required to make a request.
--
-- * 'environmentId' - The environment ID of the target environment.
-- * 'environmentName' - The name of the target environment.
-- * 'status' - To show only actions with a particular status, specify a status.
mkDescribeEnvironmentManagedActions ::
  DescribeEnvironmentManagedActions
mkDescribeEnvironmentManagedActions =
  DescribeEnvironmentManagedActions'
    { status = Lude.Nothing,
      environmentName = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | To show only actions with a particular status, specify a status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demaStatus :: Lens.Lens' DescribeEnvironmentManagedActions (Lude.Maybe ActionStatus)
demaStatus = Lens.lens (status :: DescribeEnvironmentManagedActions -> Lude.Maybe ActionStatus) (\s a -> s {status = a} :: DescribeEnvironmentManagedActions)
{-# DEPRECATED demaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the target environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demaEnvironmentName :: Lens.Lens' DescribeEnvironmentManagedActions (Lude.Maybe Lude.Text)
demaEnvironmentName = Lens.lens (environmentName :: DescribeEnvironmentManagedActions -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeEnvironmentManagedActions)
{-# DEPRECATED demaEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The environment ID of the target environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demaEnvironmentId :: Lens.Lens' DescribeEnvironmentManagedActions (Lude.Maybe Lude.Text)
demaEnvironmentId = Lens.lens (environmentId :: DescribeEnvironmentManagedActions -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: DescribeEnvironmentManagedActions)
{-# DEPRECATED demaEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest DescribeEnvironmentManagedActions where
  type
    Rs DescribeEnvironmentManagedActions =
      DescribeEnvironmentManagedActionsResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeEnvironmentManagedActionsResult"
      ( \s h x ->
          DescribeEnvironmentManagedActionsResponse'
            Lude.<$> ( x Lude..@? "ManagedActions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLNonEmpty "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEnvironmentManagedActions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEnvironmentManagedActions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEnvironmentManagedActions where
  toQuery DescribeEnvironmentManagedActions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeEnvironmentManagedActions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Status" Lude.=: status,
        "EnvironmentName" Lude.=: environmentName,
        "EnvironmentId" Lude.=: environmentId
      ]

-- | The result message containing a list of managed actions.
--
-- /See:/ 'mkDescribeEnvironmentManagedActionsResponse' smart constructor.
data DescribeEnvironmentManagedActionsResponse = DescribeEnvironmentManagedActionsResponse'
  { managedActions ::
      Lude.Maybe
        ( Lude.NonEmpty
            ManagedAction
        ),
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

-- | Creates a value of 'DescribeEnvironmentManagedActionsResponse' with the minimum fields required to make a request.
--
-- * 'managedActions' - A list of upcoming and in-progress managed actions.
-- * 'responseStatus' - The response status code.
mkDescribeEnvironmentManagedActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEnvironmentManagedActionsResponse
mkDescribeEnvironmentManagedActionsResponse pResponseStatus_ =
  DescribeEnvironmentManagedActionsResponse'
    { managedActions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of upcoming and in-progress managed actions.
--
-- /Note:/ Consider using 'managedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demarsManagedActions :: Lens.Lens' DescribeEnvironmentManagedActionsResponse (Lude.Maybe (Lude.NonEmpty ManagedAction))
demarsManagedActions = Lens.lens (managedActions :: DescribeEnvironmentManagedActionsResponse -> Lude.Maybe (Lude.NonEmpty ManagedAction)) (\s a -> s {managedActions = a} :: DescribeEnvironmentManagedActionsResponse)
{-# DEPRECATED demarsManagedActions "Use generic-lens or generic-optics with 'managedActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demarsResponseStatus :: Lens.Lens' DescribeEnvironmentManagedActionsResponse Lude.Int
demarsResponseStatus = Lens.lens (responseStatus :: DescribeEnvironmentManagedActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEnvironmentManagedActionsResponse)
{-# DEPRECATED demarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
