{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteFlowLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more flow logs.
module Network.AWS.EC2.DeleteFlowLogs
  ( -- * Creating a request
    DeleteFlowLogs (..),
    mkDeleteFlowLogs,

    -- ** Request lenses
    dflFlowLogIds,
    dflDryRun,

    -- * Destructuring the response
    DeleteFlowLogsResponse (..),
    mkDeleteFlowLogsResponse,

    -- ** Response lenses
    dflrsUnsuccessful,
    dflrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFlowLogs' smart constructor.
data DeleteFlowLogs = DeleteFlowLogs'
  { -- | One or more flow log IDs.
    --
    -- Constraint: Maximum of 1000 flow log IDs.
    flowLogIds :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFlowLogs' with the minimum fields required to make a request.
--
-- * 'flowLogIds' - One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteFlowLogs ::
  DeleteFlowLogs
mkDeleteFlowLogs =
  DeleteFlowLogs' {flowLogIds = Lude.mempty, dryRun = Lude.Nothing}

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
--
-- /Note:/ Consider using 'flowLogIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflFlowLogIds :: Lens.Lens' DeleteFlowLogs [Lude.Text]
dflFlowLogIds = Lens.lens (flowLogIds :: DeleteFlowLogs -> [Lude.Text]) (\s a -> s {flowLogIds = a} :: DeleteFlowLogs)
{-# DEPRECATED dflFlowLogIds "Use generic-lens or generic-optics with 'flowLogIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflDryRun :: Lens.Lens' DeleteFlowLogs (Lude.Maybe Lude.Bool)
dflDryRun = Lens.lens (dryRun :: DeleteFlowLogs -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteFlowLogs)
{-# DEPRECATED dflDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteFlowLogs where
  type Rs DeleteFlowLogs = DeleteFlowLogsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteFlowLogsResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFlowLogs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteFlowLogs where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteFlowLogs where
  toQuery DeleteFlowLogs' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteFlowLogs" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "FlowLogId" flowLogIds,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteFlowLogsResponse' smart constructor.
data DeleteFlowLogsResponse = DeleteFlowLogsResponse'
  { -- | Information about the flow logs that could not be deleted successfully.
    unsuccessful :: Lude.Maybe [UnsuccessfulItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFlowLogsResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessful' - Information about the flow logs that could not be deleted successfully.
-- * 'responseStatus' - The response status code.
mkDeleteFlowLogsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFlowLogsResponse
mkDeleteFlowLogsResponse pResponseStatus_ =
  DeleteFlowLogsResponse'
    { unsuccessful = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the flow logs that could not be deleted successfully.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflrsUnsuccessful :: Lens.Lens' DeleteFlowLogsResponse (Lude.Maybe [UnsuccessfulItem])
dflrsUnsuccessful = Lens.lens (unsuccessful :: DeleteFlowLogsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: DeleteFlowLogsResponse)
{-# DEPRECATED dflrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflrsResponseStatus :: Lens.Lens' DeleteFlowLogsResponse Lude.Int
dflrsResponseStatus = Lens.lens (responseStatus :: DeleteFlowLogsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFlowLogsResponse)
{-# DEPRECATED dflrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
