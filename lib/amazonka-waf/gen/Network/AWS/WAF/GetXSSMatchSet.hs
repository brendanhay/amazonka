{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetXSSMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'XssMatchSet' that is specified by @XssMatchSetId@ .
module Network.AWS.WAF.GetXSSMatchSet
  ( -- * Creating a request
    GetXSSMatchSet (..),
    mkGetXSSMatchSet,

    -- ** Request lenses
    gxmsXSSMatchSetId,

    -- * Destructuring the response
    GetXSSMatchSetResponse (..),
    mkGetXSSMatchSetResponse,

    -- ** Response lenses
    gxmsrsXSSMatchSet,
    gxmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | A request to get an 'XssMatchSet' .
--
-- /See:/ 'mkGetXSSMatchSet' smart constructor.
newtype GetXSSMatchSet = GetXSSMatchSet'
  { -- | The @XssMatchSetId@ of the 'XssMatchSet' that you want to get. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
    xssMatchSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetXSSMatchSet' with the minimum fields required to make a request.
--
-- * 'xssMatchSetId' - The @XssMatchSetId@ of the 'XssMatchSet' that you want to get. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
mkGetXSSMatchSet ::
  -- | 'xssMatchSetId'
  Lude.Text ->
  GetXSSMatchSet
mkGetXSSMatchSet pXSSMatchSetId_ =
  GetXSSMatchSet' {xssMatchSetId = pXSSMatchSetId_}

-- | The @XssMatchSetId@ of the 'XssMatchSet' that you want to get. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gxmsXSSMatchSetId :: Lens.Lens' GetXSSMatchSet Lude.Text
gxmsXSSMatchSetId = Lens.lens (xssMatchSetId :: GetXSSMatchSet -> Lude.Text) (\s a -> s {xssMatchSetId = a} :: GetXSSMatchSet)
{-# DEPRECATED gxmsXSSMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead." #-}

instance Lude.AWSRequest GetXSSMatchSet where
  type Rs GetXSSMatchSet = GetXSSMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetXSSMatchSetResponse'
            Lude.<$> (x Lude..?> "XssMatchSet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetXSSMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetXssMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetXSSMatchSet where
  toJSON GetXSSMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("XssMatchSetId" Lude..= xssMatchSetId)]
      )

instance Lude.ToPath GetXSSMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GetXSSMatchSet where
  toQuery = Lude.const Lude.mempty

-- | The response to a 'GetXssMatchSet' request.
--
-- /See:/ 'mkGetXSSMatchSetResponse' smart constructor.
data GetXSSMatchSetResponse = GetXSSMatchSetResponse'
  { -- | Information about the 'XssMatchSet' that you specified in the @GetXssMatchSet@ request. For more information, see the following topics:
    --
    --
    --     * 'XssMatchSet' : Contains @Name@ , @XssMatchSetId@ , and an array of @XssMatchTuple@ objects
    --
    --
    --     * 'XssMatchTuple' : Each @XssMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    xssMatchSet :: Lude.Maybe XSSMatchSet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetXSSMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'xssMatchSet' - Information about the 'XssMatchSet' that you specified in the @GetXssMatchSet@ request. For more information, see the following topics:
--
--
--     * 'XssMatchSet' : Contains @Name@ , @XssMatchSetId@ , and an array of @XssMatchTuple@ objects
--
--
--     * 'XssMatchTuple' : Each @XssMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
-- * 'responseStatus' - The response status code.
mkGetXSSMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetXSSMatchSetResponse
mkGetXSSMatchSetResponse pResponseStatus_ =
  GetXSSMatchSetResponse'
    { xssMatchSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'XssMatchSet' that you specified in the @GetXssMatchSet@ request. For more information, see the following topics:
--
--
--     * 'XssMatchSet' : Contains @Name@ , @XssMatchSetId@ , and an array of @XssMatchTuple@ objects
--
--
--     * 'XssMatchTuple' : Each @XssMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'xssMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gxmsrsXSSMatchSet :: Lens.Lens' GetXSSMatchSetResponse (Lude.Maybe XSSMatchSet)
gxmsrsXSSMatchSet = Lens.lens (xssMatchSet :: GetXSSMatchSetResponse -> Lude.Maybe XSSMatchSet) (\s a -> s {xssMatchSet = a} :: GetXSSMatchSetResponse)
{-# DEPRECATED gxmsrsXSSMatchSet "Use generic-lens or generic-optics with 'xssMatchSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gxmsrsResponseStatus :: Lens.Lens' GetXSSMatchSetResponse Lude.Int
gxmsrsResponseStatus = Lens.lens (responseStatus :: GetXSSMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetXSSMatchSetResponse)
{-# DEPRECATED gxmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
