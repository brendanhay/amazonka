{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeOperatingSystems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the operating systems that are supported by AWS OpsWorks Stacks.
module Network.AWS.OpsWorks.DescribeOperatingSystems
  ( -- * Creating a request
    DescribeOperatingSystems (..),
    mkDescribeOperatingSystems,

    -- * Destructuring the response
    DescribeOperatingSystemsResponse (..),
    mkDescribeOperatingSystemsResponse,

    -- ** Response lenses
    dosrsOperatingSystems,
    dosrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOperatingSystems' smart constructor.
data DescribeOperatingSystems = DescribeOperatingSystems'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOperatingSystems' with the minimum fields required to make a request.
mkDescribeOperatingSystems ::
  DescribeOperatingSystems
mkDescribeOperatingSystems = DescribeOperatingSystems'

instance Lude.AWSRequest DescribeOperatingSystems where
  type Rs DescribeOperatingSystems = DescribeOperatingSystemsResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOperatingSystemsResponse'
            Lude.<$> (x Lude..?> "OperatingSystems" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOperatingSystems where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeOperatingSystems" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOperatingSystems where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeOperatingSystems where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOperatingSystems where
  toQuery = Lude.const Lude.mempty

-- | The response to a @DescribeOperatingSystems@ request.
--
-- /See:/ 'mkDescribeOperatingSystemsResponse' smart constructor.
data DescribeOperatingSystemsResponse = DescribeOperatingSystemsResponse'
  { -- | Contains information in response to a @DescribeOperatingSystems@ request.
    operatingSystems :: Lude.Maybe [OperatingSystem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOperatingSystemsResponse' with the minimum fields required to make a request.
--
-- * 'operatingSystems' - Contains information in response to a @DescribeOperatingSystems@ request.
-- * 'responseStatus' - The response status code.
mkDescribeOperatingSystemsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOperatingSystemsResponse
mkDescribeOperatingSystemsResponse pResponseStatus_ =
  DescribeOperatingSystemsResponse'
    { operatingSystems =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains information in response to a @DescribeOperatingSystems@ request.
--
-- /Note:/ Consider using 'operatingSystems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dosrsOperatingSystems :: Lens.Lens' DescribeOperatingSystemsResponse (Lude.Maybe [OperatingSystem])
dosrsOperatingSystems = Lens.lens (operatingSystems :: DescribeOperatingSystemsResponse -> Lude.Maybe [OperatingSystem]) (\s a -> s {operatingSystems = a} :: DescribeOperatingSystemsResponse)
{-# DEPRECATED dosrsOperatingSystems "Use generic-lens or generic-optics with 'operatingSystems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dosrsResponseStatus :: Lens.Lens' DescribeOperatingSystemsResponse Lude.Int
dosrsResponseStatus = Lens.lens (responseStatus :: DescribeOperatingSystemsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOperatingSystemsResponse)
{-# DEPRECATED dosrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
