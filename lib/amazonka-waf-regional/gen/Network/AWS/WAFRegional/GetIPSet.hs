{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'IPSet' that is specified by @IPSetId@ .
module Network.AWS.WAFRegional.GetIPSet
  ( -- * Creating a request
    GetIPSet (..),
    mkGetIPSet,

    -- ** Request lenses
    gisIPSetId,

    -- * Destructuring the response
    GetIPSetResponse (..),
    mkGetIPSetResponse,

    -- ** Response lenses
    gisrsIPSet,
    gisrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkGetIPSet' smart constructor.
newtype GetIPSet = GetIPSet'
  { -- | The @IPSetId@ of the 'IPSet' that you want to get. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
    ipSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIPSet' with the minimum fields required to make a request.
--
-- * 'ipSetId' - The @IPSetId@ of the 'IPSet' that you want to get. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
mkGetIPSet ::
  -- | 'ipSetId'
  Lude.Text ->
  GetIPSet
mkGetIPSet pIPSetId_ = GetIPSet' {ipSetId = pIPSetId_}

-- | The @IPSetId@ of the 'IPSet' that you want to get. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisIPSetId :: Lens.Lens' GetIPSet Lude.Text
gisIPSetId = Lens.lens (ipSetId :: GetIPSet -> Lude.Text) (\s a -> s {ipSetId = a} :: GetIPSet)
{-# DEPRECATED gisIPSetId "Use generic-lens or generic-optics with 'ipSetId' instead." #-}

instance Lude.AWSRequest GetIPSet where
  type Rs GetIPSet = GetIPSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetIPSetResponse'
            Lude.<$> (x Lude..?> "IPSet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIPSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.GetIPSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetIPSet where
  toJSON GetIPSet' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("IPSetId" Lude..= ipSetId)])

instance Lude.ToPath GetIPSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GetIPSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetIPSetResponse' smart constructor.
data GetIPSetResponse = GetIPSetResponse'
  { -- | Information about the 'IPSet' that you specified in the @GetIPSet@ request. For more information, see the following topics:
    --
    --
    --     * 'IPSet' : Contains @IPSetDescriptors@ , @IPSetId@ , and @Name@
    --
    --
    --     * @IPSetDescriptors@ : Contains an array of 'IPSetDescriptor' objects. Each @IPSetDescriptor@ object contains @Type@ and @Value@
    ipSet :: Lude.Maybe IPSet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIPSetResponse' with the minimum fields required to make a request.
--
-- * 'ipSet' - Information about the 'IPSet' that you specified in the @GetIPSet@ request. For more information, see the following topics:
--
--
--     * 'IPSet' : Contains @IPSetDescriptors@ , @IPSetId@ , and @Name@
--
--
--     * @IPSetDescriptors@ : Contains an array of 'IPSetDescriptor' objects. Each @IPSetDescriptor@ object contains @Type@ and @Value@
--
--
-- * 'responseStatus' - The response status code.
mkGetIPSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIPSetResponse
mkGetIPSetResponse pResponseStatus_ =
  GetIPSetResponse'
    { ipSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'IPSet' that you specified in the @GetIPSet@ request. For more information, see the following topics:
--
--
--     * 'IPSet' : Contains @IPSetDescriptors@ , @IPSetId@ , and @Name@
--
--
--     * @IPSetDescriptors@ : Contains an array of 'IPSetDescriptor' objects. Each @IPSetDescriptor@ object contains @Type@ and @Value@
--
--
--
-- /Note:/ Consider using 'ipSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsIPSet :: Lens.Lens' GetIPSetResponse (Lude.Maybe IPSet)
gisrsIPSet = Lens.lens (ipSet :: GetIPSetResponse -> Lude.Maybe IPSet) (\s a -> s {ipSet = a} :: GetIPSetResponse)
{-# DEPRECATED gisrsIPSet "Use generic-lens or generic-optics with 'ipSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsResponseStatus :: Lens.Lens' GetIPSetResponse Lude.Int
gisrsResponseStatus = Lens.lens (responseStatus :: GetIPSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIPSetResponse)
{-# DEPRECATED gisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
