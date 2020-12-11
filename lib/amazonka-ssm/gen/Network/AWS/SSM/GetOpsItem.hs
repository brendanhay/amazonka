{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetOpsItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about an OpsItem by using the ID. You must have permission in AWS Identity and Access Management (IAM) to view information about an OpsItem. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.GetOpsItem
  ( -- * Creating a request
    GetOpsItem (..),
    mkGetOpsItem,

    -- ** Request lenses
    goiOpsItemId,

    -- * Destructuring the response
    GetOpsItemResponse (..),
    mkGetOpsItemResponse,

    -- ** Response lenses
    goirsOpsItem,
    goirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetOpsItem' smart constructor.
newtype GetOpsItem = GetOpsItem' {opsItemId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOpsItem' with the minimum fields required to make a request.
--
-- * 'opsItemId' - The ID of the OpsItem that you want to get.
mkGetOpsItem ::
  -- | 'opsItemId'
  Lude.Text ->
  GetOpsItem
mkGetOpsItem pOpsItemId_ = GetOpsItem' {opsItemId = pOpsItemId_}

-- | The ID of the OpsItem that you want to get.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiOpsItemId :: Lens.Lens' GetOpsItem Lude.Text
goiOpsItemId = Lens.lens (opsItemId :: GetOpsItem -> Lude.Text) (\s a -> s {opsItemId = a} :: GetOpsItem)
{-# DEPRECATED goiOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

instance Lude.AWSRequest GetOpsItem where
  type Rs GetOpsItem = GetOpsItemResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOpsItemResponse'
            Lude.<$> (x Lude..?> "OpsItem") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOpsItem where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetOpsItem" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOpsItem where
  toJSON GetOpsItem' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("OpsItemId" Lude..= opsItemId)])

instance Lude.ToPath GetOpsItem where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOpsItem where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOpsItemResponse' smart constructor.
data GetOpsItemResponse = GetOpsItemResponse'
  { opsItem ::
      Lude.Maybe OpsItem,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOpsItemResponse' with the minimum fields required to make a request.
--
-- * 'opsItem' - The OpsItem.
-- * 'responseStatus' - The response status code.
mkGetOpsItemResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOpsItemResponse
mkGetOpsItemResponse pResponseStatus_ =
  GetOpsItemResponse'
    { opsItem = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The OpsItem.
--
-- /Note:/ Consider using 'opsItem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirsOpsItem :: Lens.Lens' GetOpsItemResponse (Lude.Maybe OpsItem)
goirsOpsItem = Lens.lens (opsItem :: GetOpsItemResponse -> Lude.Maybe OpsItem) (\s a -> s {opsItem = a} :: GetOpsItemResponse)
{-# DEPRECATED goirsOpsItem "Use generic-lens or generic-optics with 'opsItem' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirsResponseStatus :: Lens.Lens' GetOpsItemResponse Lude.Int
goirsResponseStatus = Lens.lens (responseStatus :: GetOpsItemResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOpsItemResponse)
{-# DEPRECATED goirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
