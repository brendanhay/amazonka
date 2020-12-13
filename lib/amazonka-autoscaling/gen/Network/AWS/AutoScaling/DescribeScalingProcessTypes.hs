{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScalingProcessTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scaling process types for use with the 'ResumeProcesses' and 'SuspendProcesses' APIs.
module Network.AWS.AutoScaling.DescribeScalingProcessTypes
  ( -- * Creating a request
    DescribeScalingProcessTypes (..),
    mkDescribeScalingProcessTypes,

    -- * Destructuring the response
    DescribeScalingProcessTypesResponse (..),
    mkDescribeScalingProcessTypesResponse,

    -- ** Response lenses
    dsptrsProcesses,
    dsptrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScalingProcessTypes' smart constructor.
data DescribeScalingProcessTypes = DescribeScalingProcessTypes'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingProcessTypes' with the minimum fields required to make a request.
mkDescribeScalingProcessTypes ::
  DescribeScalingProcessTypes
mkDescribeScalingProcessTypes = DescribeScalingProcessTypes'

instance Lude.AWSRequest DescribeScalingProcessTypes where
  type
    Rs DescribeScalingProcessTypes =
      DescribeScalingProcessTypesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeScalingProcessTypesResult"
      ( \s h x ->
          DescribeScalingProcessTypesResponse'
            Lude.<$> ( x Lude..@? "Processes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScalingProcessTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeScalingProcessTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScalingProcessTypes where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("DescribeScalingProcessTypes" :: Lude.ByteString),
            "Version" Lude.=: ("2011-01-01" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDescribeScalingProcessTypesResponse' smart constructor.
data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse'
  { -- | The names of the process types.
    processes :: Lude.Maybe [ProcessType],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingProcessTypesResponse' with the minimum fields required to make a request.
--
-- * 'processes' - The names of the process types.
-- * 'responseStatus' - The response status code.
mkDescribeScalingProcessTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScalingProcessTypesResponse
mkDescribeScalingProcessTypesResponse pResponseStatus_ =
  DescribeScalingProcessTypesResponse'
    { processes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of the process types.
--
-- /Note:/ Consider using 'processes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsptrsProcesses :: Lens.Lens' DescribeScalingProcessTypesResponse (Lude.Maybe [ProcessType])
dsptrsProcesses = Lens.lens (processes :: DescribeScalingProcessTypesResponse -> Lude.Maybe [ProcessType]) (\s a -> s {processes = a} :: DescribeScalingProcessTypesResponse)
{-# DEPRECATED dsptrsProcesses "Use generic-lens or generic-optics with 'processes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsptrsResponseStatus :: Lens.Lens' DescribeScalingProcessTypesResponse Lude.Int
dsptrsResponseStatus = Lens.lens (responseStatus :: DescribeScalingProcessTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScalingProcessTypesResponse)
{-# DEPRECATED dsptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
