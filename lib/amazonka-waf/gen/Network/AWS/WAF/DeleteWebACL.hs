{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'WebACL' . You can't delete a @WebACL@ if it still contains any @Rules@ .
--
-- To delete a @WebACL@ , perform the following steps:
--
--     * Update the @WebACL@ to remove @Rules@ , if any. For more information, see 'UpdateWebACL' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteWebACL@ request.
--
--
--     * Submit a @DeleteWebACL@ request.
module Network.AWS.WAF.DeleteWebACL
  ( -- * Creating a request
    DeleteWebACL (..),
    mkDeleteWebACL,

    -- ** Request lenses
    dwaWebACLId,
    dwaChangeToken,

    -- * Destructuring the response
    DeleteWebACLResponse (..),
    mkDeleteWebACLResponse,

    -- ** Response lenses
    dwarsChangeToken,
    dwarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkDeleteWebACL' smart constructor.
data DeleteWebACL = DeleteWebACL'
  { -- | The @WebACLId@ of the 'WebACL' that you want to delete. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
    webACLId :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWebACL' with the minimum fields required to make a request.
--
-- * 'webACLId' - The @WebACLId@ of the 'WebACL' that you want to delete. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkDeleteWebACL ::
  -- | 'webACLId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteWebACL
mkDeleteWebACL pWebACLId_ pChangeToken_ =
  DeleteWebACL' {webACLId = pWebACLId_, changeToken = pChangeToken_}

-- | The @WebACLId@ of the 'WebACL' that you want to delete. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwaWebACLId :: Lens.Lens' DeleteWebACL Lude.Text
dwaWebACLId = Lens.lens (webACLId :: DeleteWebACL -> Lude.Text) (\s a -> s {webACLId = a} :: DeleteWebACL)
{-# DEPRECATED dwaWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwaChangeToken :: Lens.Lens' DeleteWebACL Lude.Text
dwaChangeToken = Lens.lens (changeToken :: DeleteWebACL -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteWebACL)
{-# DEPRECATED dwaChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteWebACL where
  type Rs DeleteWebACL = DeleteWebACLResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteWebACLResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteWebACL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.DeleteWebACL" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteWebACL where
  toJSON DeleteWebACL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WebACLId" Lude..= webACLId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteWebACL where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteWebACL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteWebACLResponse' smart constructor.
data DeleteWebACLResponse = DeleteWebACLResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWebACLResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteWebACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteWebACLResponse
mkDeleteWebACLResponse pResponseStatus_ =
  DeleteWebACLResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwarsChangeToken :: Lens.Lens' DeleteWebACLResponse (Lude.Maybe Lude.Text)
dwarsChangeToken = Lens.lens (changeToken :: DeleteWebACLResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteWebACLResponse)
{-# DEPRECATED dwarsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwarsResponseStatus :: Lens.Lens' DeleteWebACLResponse Lude.Int
dwarsResponseStatus = Lens.lens (responseStatus :: DeleteWebACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteWebACLResponse)
{-# DEPRECATED dwarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
