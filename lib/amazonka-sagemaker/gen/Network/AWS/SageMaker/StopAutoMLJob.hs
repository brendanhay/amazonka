{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A method for forcing the termination of a running job.
module Network.AWS.SageMaker.StopAutoMLJob
  ( -- * Creating a request
    StopAutoMLJob (..),
    mkStopAutoMLJob,

    -- ** Request lenses
    samljAutoMLJobName,

    -- * Destructuring the response
    StopAutoMLJobResponse (..),
    mkStopAutoMLJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStopAutoMLJob' smart constructor.
newtype StopAutoMLJob = StopAutoMLJob' {autoMLJobName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAutoMLJob' with the minimum fields required to make a request.
--
-- * 'autoMLJobName' - The name of the object you are requesting.
mkStopAutoMLJob ::
  -- | 'autoMLJobName'
  Lude.Text ->
  StopAutoMLJob
mkStopAutoMLJob pAutoMLJobName_ =
  StopAutoMLJob' {autoMLJobName = pAutoMLJobName_}

-- | The name of the object you are requesting.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samljAutoMLJobName :: Lens.Lens' StopAutoMLJob Lude.Text
samljAutoMLJobName = Lens.lens (autoMLJobName :: StopAutoMLJob -> Lude.Text) (\s a -> s {autoMLJobName = a} :: StopAutoMLJob)
{-# DEPRECATED samljAutoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead." #-}

instance Lude.AWSRequest StopAutoMLJob where
  type Rs StopAutoMLJob = StopAutoMLJobResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StopAutoMLJobResponse'

instance Lude.ToHeaders StopAutoMLJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StopAutoMLJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopAutoMLJob where
  toJSON StopAutoMLJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AutoMLJobName" Lude..= autoMLJobName)]
      )

instance Lude.ToPath StopAutoMLJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopAutoMLJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopAutoMLJobResponse' smart constructor.
data StopAutoMLJobResponse = StopAutoMLJobResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAutoMLJobResponse' with the minimum fields required to make a request.
mkStopAutoMLJobResponse ::
  StopAutoMLJobResponse
mkStopAutoMLJobResponse = StopAutoMLJobResponse'
