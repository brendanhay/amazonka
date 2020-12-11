{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteResourcesByExternalId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes resources linked to an external ID.
module Network.AWS.CodeDeploy.DeleteResourcesByExternalId
  ( -- * Creating a request
    DeleteResourcesByExternalId (..),
    mkDeleteResourcesByExternalId,

    -- ** Request lenses
    drbeiExternalId,

    -- * Destructuring the response
    DeleteResourcesByExternalIdResponse (..),
    mkDeleteResourcesByExternalIdResponse,

    -- ** Response lenses
    drbeirsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteResourcesByExternalId' smart constructor.
newtype DeleteResourcesByExternalId = DeleteResourcesByExternalId'
  { externalId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourcesByExternalId' with the minimum fields required to make a request.
--
-- * 'externalId' - The unique ID of an external resource (for example, a CloudFormation stack ID) that is linked to one or more CodeDeploy resources.
mkDeleteResourcesByExternalId ::
  DeleteResourcesByExternalId
mkDeleteResourcesByExternalId =
  DeleteResourcesByExternalId' {externalId = Lude.Nothing}

-- | The unique ID of an external resource (for example, a CloudFormation stack ID) that is linked to one or more CodeDeploy resources.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbeiExternalId :: Lens.Lens' DeleteResourcesByExternalId (Lude.Maybe Lude.Text)
drbeiExternalId = Lens.lens (externalId :: DeleteResourcesByExternalId -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: DeleteResourcesByExternalId)
{-# DEPRECATED drbeiExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

instance Lude.AWSRequest DeleteResourcesByExternalId where
  type
    Rs DeleteResourcesByExternalId =
      DeleteResourcesByExternalIdResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteResourcesByExternalIdResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteResourcesByExternalId where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.DeleteResourcesByExternalId" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteResourcesByExternalId where
  toJSON DeleteResourcesByExternalId' {..} =
    Lude.object
      (Lude.catMaybes [("externalId" Lude..=) Lude.<$> externalId])

instance Lude.ToPath DeleteResourcesByExternalId where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteResourcesByExternalId where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourcesByExternalIdResponse' smart constructor.
newtype DeleteResourcesByExternalIdResponse = DeleteResourcesByExternalIdResponse'
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

-- | Creates a value of 'DeleteResourcesByExternalIdResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteResourcesByExternalIdResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteResourcesByExternalIdResponse
mkDeleteResourcesByExternalIdResponse pResponseStatus_ =
  DeleteResourcesByExternalIdResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbeirsResponseStatus :: Lens.Lens' DeleteResourcesByExternalIdResponse Lude.Int
drbeirsResponseStatus = Lens.lens (responseStatus :: DeleteResourcesByExternalIdResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteResourcesByExternalIdResponse)
{-# DEPRECATED drbeirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
