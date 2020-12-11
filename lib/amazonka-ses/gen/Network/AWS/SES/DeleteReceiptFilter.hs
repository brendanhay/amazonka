{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteReceiptFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IP address filter.
--
-- For information about managing IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteReceiptFilter
  ( -- * Creating a request
    DeleteReceiptFilter (..),
    mkDeleteReceiptFilter,

    -- ** Request lenses
    drfFilterName,

    -- * Destructuring the response
    DeleteReceiptFilterResponse (..),
    mkDeleteReceiptFilterResponse,

    -- ** Response lenses
    drfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete an IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteReceiptFilter' smart constructor.
newtype DeleteReceiptFilter = DeleteReceiptFilter'
  { filterName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReceiptFilter' with the minimum fields required to make a request.
--
-- * 'filterName' - The name of the IP address filter to delete.
mkDeleteReceiptFilter ::
  -- | 'filterName'
  Lude.Text ->
  DeleteReceiptFilter
mkDeleteReceiptFilter pFilterName_ =
  DeleteReceiptFilter' {filterName = pFilterName_}

-- | The name of the IP address filter to delete.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfFilterName :: Lens.Lens' DeleteReceiptFilter Lude.Text
drfFilterName = Lens.lens (filterName :: DeleteReceiptFilter -> Lude.Text) (\s a -> s {filterName = a} :: DeleteReceiptFilter)
{-# DEPRECATED drfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

instance Lude.AWSRequest DeleteReceiptFilter where
  type Rs DeleteReceiptFilter = DeleteReceiptFilterResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DeleteReceiptFilterResult"
      ( \s h x ->
          DeleteReceiptFilterResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReceiptFilter where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteReceiptFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReceiptFilter where
  toQuery DeleteReceiptFilter' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteReceiptFilter" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "FilterName" Lude.=: filterName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteReceiptFilterResponse' smart constructor.
newtype DeleteReceiptFilterResponse = DeleteReceiptFilterResponse'
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

-- | Creates a value of 'DeleteReceiptFilterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteReceiptFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReceiptFilterResponse
mkDeleteReceiptFilterResponse pResponseStatus_ =
  DeleteReceiptFilterResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsResponseStatus :: Lens.Lens' DeleteReceiptFilterResponse Lude.Int
drfrsResponseStatus = Lens.lens (responseStatus :: DeleteReceiptFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReceiptFilterResponse)
{-# DEPRECATED drfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
