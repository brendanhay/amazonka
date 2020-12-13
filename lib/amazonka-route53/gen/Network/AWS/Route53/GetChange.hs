{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of a change batch request. The status is one of the following values:
--
--
--     * @PENDING@ indicates that the changes in this request have not propagated to all Amazon Route 53 DNS servers. This is the initial status of all change batch requests.
--
--
--     * @INSYNC@ indicates that the changes have propagated to all Route 53 DNS servers.
module Network.AWS.Route53.GetChange
  ( -- * Creating a request
    GetChange (..),
    mkGetChange,

    -- ** Request lenses
    gcId,

    -- * Destructuring the response
    GetChangeResponse (..),
    mkGetChangeResponse,

    -- ** Response lenses
    gcrsChangeInfo,
    gcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | The input for a GetChange request.
--
-- /See:/ 'mkGetChange' smart constructor.
newtype GetChange = GetChange'
  { -- | The ID of the change batch request. The value that you specify here is the value that @ChangeResourceRecordSets@ returned in the @Id@ element when you submitted the request.
    id :: ResourceId
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetChange' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the change batch request. The value that you specify here is the value that @ChangeResourceRecordSets@ returned in the @Id@ element when you submitted the request.
mkGetChange ::
  -- | 'id'
  ResourceId ->
  GetChange
mkGetChange pId_ = GetChange' {id = pId_}

-- | The ID of the change batch request. The value that you specify here is the value that @ChangeResourceRecordSets@ returned in the @Id@ element when you submitted the request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcId :: Lens.Lens' GetChange ResourceId
gcId = Lens.lens (id :: GetChange -> ResourceId) (\s a -> s {id = a} :: GetChange)
{-# DEPRECATED gcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetChange where
  type Rs GetChange = GetChangeResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetChangeResponse'
            Lude.<$> (x Lude..@ "ChangeInfo") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetChange where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetChange where
  toPath GetChange' {..} =
    Lude.mconcat ["/2013-04-01/change/", Lude.toBS id]

instance Lude.ToQuery GetChange where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the @ChangeInfo@ element.
--
-- /See:/ 'mkGetChangeResponse' smart constructor.
data GetChangeResponse = GetChangeResponse'
  { -- | A complex type that contains information about the specified change batch.
    changeInfo :: ChangeInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetChangeResponse' with the minimum fields required to make a request.
--
-- * 'changeInfo' - A complex type that contains information about the specified change batch.
-- * 'responseStatus' - The response status code.
mkGetChangeResponse ::
  -- | 'changeInfo'
  ChangeInfo ->
  -- | 'responseStatus'
  Lude.Int ->
  GetChangeResponse
mkGetChangeResponse pChangeInfo_ pResponseStatus_ =
  GetChangeResponse'
    { changeInfo = pChangeInfo_,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains information about the specified change batch.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsChangeInfo :: Lens.Lens' GetChangeResponse ChangeInfo
gcrsChangeInfo = Lens.lens (changeInfo :: GetChangeResponse -> ChangeInfo) (\s a -> s {changeInfo = a} :: GetChangeResponse)
{-# DEPRECATED gcrsChangeInfo "Use generic-lens or generic-optics with 'changeInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetChangeResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetChangeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetChangeResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
