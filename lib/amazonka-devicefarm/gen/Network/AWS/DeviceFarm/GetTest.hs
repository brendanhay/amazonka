{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a test.
module Network.AWS.DeviceFarm.GetTest
  ( -- * Creating a request
    GetTest (..),
    mkGetTest,

    -- ** Request lenses
    gtArn,

    -- * Destructuring the response
    GetTestResponse (..),
    mkGetTestResponse,

    -- ** Response lenses
    gtrsTest,
    gtrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the get test operation.
--
-- /See:/ 'mkGetTest' smart constructor.
newtype GetTest = GetTest'
  { -- | The test's ARN.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTest' with the minimum fields required to make a request.
--
-- * 'arn' - The test's ARN.
mkGetTest ::
  -- | 'arn'
  Lude.Text ->
  GetTest
mkGetTest pArn_ = GetTest' {arn = pArn_}

-- | The test's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtArn :: Lens.Lens' GetTest Lude.Text
gtArn = Lens.lens (arn :: GetTest -> Lude.Text) (\s a -> s {arn = a} :: GetTest)
{-# DEPRECATED gtArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetTest where
  type Rs GetTest = GetTestResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTestResponse'
            Lude.<$> (x Lude..?> "test") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetTest" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTest where
  toJSON GetTest' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetTest where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTest where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a get test request.
--
-- /See:/ 'mkGetTestResponse' smart constructor.
data GetTestResponse = GetTestResponse'
  { -- | A test condition that is evaluated.
    test :: Lude.Maybe Test,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTestResponse' with the minimum fields required to make a request.
--
-- * 'test' - A test condition that is evaluated.
-- * 'responseStatus' - The response status code.
mkGetTestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTestResponse
mkGetTestResponse pResponseStatus_ =
  GetTestResponse'
    { test = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A test condition that is evaluated.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsTest :: Lens.Lens' GetTestResponse (Lude.Maybe Test)
gtrsTest = Lens.lens (test :: GetTestResponse -> Lude.Maybe Test) (\s a -> s {test = a} :: GetTestResponse)
{-# DEPRECATED gtrsTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsResponseStatus :: Lens.Lens' GetTestResponse Lude.Int
gtrsResponseStatus = Lens.lens (responseStatus :: GetTestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTestResponse)
{-# DEPRECATED gtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
