{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetSuite
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a suite.
module Network.AWS.DeviceFarm.GetSuite
  ( -- * Creating a request
    GetSuite (..),
    mkGetSuite,

    -- ** Request lenses
    gsArn,

    -- * Destructuring the response
    GetSuiteResponse (..),
    mkGetSuiteResponse,

    -- ** Response lenses
    gsrsSuite,
    gsrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the get suite operation.
--
-- /See:/ 'mkGetSuite' smart constructor.
newtype GetSuite = GetSuite'
  { -- | The suite's ARN.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSuite' with the minimum fields required to make a request.
--
-- * 'arn' - The suite's ARN.
mkGetSuite ::
  -- | 'arn'
  Lude.Text ->
  GetSuite
mkGetSuite pArn_ = GetSuite' {arn = pArn_}

-- | The suite's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsArn :: Lens.Lens' GetSuite Lude.Text
gsArn = Lens.lens (arn :: GetSuite -> Lude.Text) (\s a -> s {arn = a} :: GetSuite)
{-# DEPRECATED gsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetSuite where
  type Rs GetSuite = GetSuiteResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSuiteResponse'
            Lude.<$> (x Lude..?> "suite") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSuite where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetSuite" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSuite where
  toJSON GetSuite' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetSuite where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSuite where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a get suite request.
--
-- /See:/ 'mkGetSuiteResponse' smart constructor.
data GetSuiteResponse = GetSuiteResponse'
  { -- | A collection of one or more tests.
    suite :: Lude.Maybe Suite,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSuiteResponse' with the minimum fields required to make a request.
--
-- * 'suite' - A collection of one or more tests.
-- * 'responseStatus' - The response status code.
mkGetSuiteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSuiteResponse
mkGetSuiteResponse pResponseStatus_ =
  GetSuiteResponse'
    { suite = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of one or more tests.
--
-- /Note:/ Consider using 'suite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSuite :: Lens.Lens' GetSuiteResponse (Lude.Maybe Suite)
gsrsSuite = Lens.lens (suite :: GetSuiteResponse -> Lude.Maybe Suite) (\s a -> s {suite = a} :: GetSuiteResponse)
{-# DEPRECATED gsrsSuite "Use generic-lens or generic-optics with 'suite' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetSuiteResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetSuiteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSuiteResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
