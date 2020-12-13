{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.StopLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends the recording of AWS API calls and log file delivery for the specified trail. Under most circumstances, there is no need to use this action. You can update a trail without stopping it first. This action is the only way to stop recording. For a trail enabled in all regions, this operation must be called from the region in which the trail was created, or an @InvalidHomeRegionException@ will occur. This operation cannot be called on the shadow trails (replicated trails in other regions) of a trail enabled in all regions.
module Network.AWS.CloudTrail.StopLogging
  ( -- * Creating a request
    StopLogging (..),
    mkStopLogging,

    -- ** Request lenses
    slName,

    -- * Destructuring the response
    StopLoggingResponse (..),
    mkStopLoggingResponse,

    -- ** Response lenses
    slrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Passes the request to CloudTrail to stop logging AWS API calls for the specified account.
--
-- /See:/ 'mkStopLogging' smart constructor.
newtype StopLogging = StopLogging'
  { -- | Specifies the name or the CloudTrail ARN of the trail for which CloudTrail will stop logging AWS API calls. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopLogging' with the minimum fields required to make a request.
--
-- * 'name' - Specifies the name or the CloudTrail ARN of the trail for which CloudTrail will stop logging AWS API calls. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
mkStopLogging ::
  -- | 'name'
  Lude.Text ->
  StopLogging
mkStopLogging pName_ = StopLogging' {name = pName_}

-- | Specifies the name or the CloudTrail ARN of the trail for which CloudTrail will stop logging AWS API calls. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slName :: Lens.Lens' StopLogging Lude.Text
slName = Lens.lens (name :: StopLogging -> Lude.Text) (\s a -> s {name = a} :: StopLogging)
{-# DEPRECATED slName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StopLogging where
  type Rs StopLogging = StopLoggingResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopLoggingResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopLogging where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StopLogging" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopLogging where
  toJSON StopLogging' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StopLogging where
  toPath = Lude.const "/"

instance Lude.ToQuery StopLogging where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkStopLoggingResponse' smart constructor.
newtype StopLoggingResponse = StopLoggingResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopLoggingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopLoggingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopLoggingResponse
mkStopLoggingResponse pResponseStatus_ =
  StopLoggingResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slrsResponseStatus :: Lens.Lens' StopLoggingResponse Lude.Int
slrsResponseStatus = Lens.lens (responseStatus :: StopLoggingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopLoggingResponse)
{-# DEPRECATED slrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
