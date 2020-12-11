{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.StartLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the recording of AWS API calls and log file delivery for a trail. For a trail that is enabled in all regions, this operation must be called from the region in which the trail was created. This operation cannot be called on the shadow trails (replicated trails in other regions) of a trail that is enabled in all regions.
module Network.AWS.CloudTrail.StartLogging
  ( -- * Creating a request
    StartLogging (..),
    mkStartLogging,

    -- ** Request lenses
    sName,

    -- * Destructuring the response
    StartLoggingResponse (..),
    mkStartLoggingResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to CloudTrail to start logging AWS API calls for an account.
--
-- /See:/ 'mkStartLogging' smart constructor.
newtype StartLogging = StartLogging' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartLogging' with the minimum fields required to make a request.
--
-- * 'name' - Specifies the name or the CloudTrail ARN of the trail for which CloudTrail logs AWS API calls. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
mkStartLogging ::
  -- | 'name'
  Lude.Text ->
  StartLogging
mkStartLogging pName_ = StartLogging' {name = pName_}

-- | Specifies the name or the CloudTrail ARN of the trail for which CloudTrail logs AWS API calls. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' StartLogging Lude.Text
sName = Lens.lens (name :: StartLogging -> Lude.Text) (\s a -> s {name = a} :: StartLogging)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StartLogging where
  type Rs StartLogging = StartLoggingResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartLoggingResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartLogging where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StartLogging" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartLogging where
  toJSON StartLogging' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StartLogging where
  toPath = Lude.const "/"

instance Lude.ToQuery StartLogging where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkStartLoggingResponse' smart constructor.
newtype StartLoggingResponse = StartLoggingResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartLoggingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartLoggingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartLoggingResponse
mkStartLoggingResponse pResponseStatus_ =
  StartLoggingResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartLoggingResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartLoggingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartLoggingResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
