{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.DeleteTrail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a trail. This operation must be called from the region in which the trail was created. @DeleteTrail@ cannot be called on the shadow trails (replicated trails in other regions) of a trail that is enabled in all regions.
module Network.AWS.CloudTrail.DeleteTrail
  ( -- * Creating a request
    DeleteTrail (..),
    mkDeleteTrail,

    -- ** Request lenses
    dtName,

    -- * Destructuring the response
    DeleteTrailResponse (..),
    mkDeleteTrailResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request that specifies the name of a trail to delete.
--
-- /See:/ 'mkDeleteTrail' smart constructor.
newtype DeleteTrail = DeleteTrail' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrail' with the minimum fields required to make a request.
--
-- * 'name' - Specifies the name or the CloudTrail ARN of the trail to be deleted. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
mkDeleteTrail ::
  -- | 'name'
  Lude.Text ->
  DeleteTrail
mkDeleteTrail pName_ = DeleteTrail' {name = pName_}

-- | Specifies the name or the CloudTrail ARN of the trail to be deleted. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtName :: Lens.Lens' DeleteTrail Lude.Text
dtName = Lens.lens (name :: DeleteTrail -> Lude.Text) (\s a -> s {name = a} :: DeleteTrail)
{-# DEPRECATED dtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteTrail where
  type Rs DeleteTrail = DeleteTrailResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTrailResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DeleteTrail" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTrail where
  toJSON DeleteTrail' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteTrail where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTrail where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkDeleteTrailResponse' smart constructor.
newtype DeleteTrailResponse = DeleteTrailResponse'
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

-- | Creates a value of 'DeleteTrailResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTrailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTrailResponse
mkDeleteTrailResponse pResponseStatus_ =
  DeleteTrailResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteTrailResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteTrailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTrailResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
