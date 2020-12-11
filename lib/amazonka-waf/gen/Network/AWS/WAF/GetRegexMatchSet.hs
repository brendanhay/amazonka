{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RegexMatchSet' specified by @RegexMatchSetId@ .
module Network.AWS.WAF.GetRegexMatchSet
  ( -- * Creating a request
    GetRegexMatchSet (..),
    mkGetRegexMatchSet,

    -- ** Request lenses
    grmsRegexMatchSetId,

    -- * Destructuring the response
    GetRegexMatchSetResponse (..),
    mkGetRegexMatchSetResponse,

    -- ** Response lenses
    grmsrsRegexMatchSet,
    grmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkGetRegexMatchSet' smart constructor.
newtype GetRegexMatchSet = GetRegexMatchSet'
  { regexMatchSetId ::
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

-- | Creates a value of 'GetRegexMatchSet' with the minimum fields required to make a request.
--
-- * 'regexMatchSetId' - The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to get. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
mkGetRegexMatchSet ::
  -- | 'regexMatchSetId'
  Lude.Text ->
  GetRegexMatchSet
mkGetRegexMatchSet pRegexMatchSetId_ =
  GetRegexMatchSet' {regexMatchSetId = pRegexMatchSetId_}

-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to get. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grmsRegexMatchSetId :: Lens.Lens' GetRegexMatchSet Lude.Text
grmsRegexMatchSetId = Lens.lens (regexMatchSetId :: GetRegexMatchSet -> Lude.Text) (\s a -> s {regexMatchSetId = a} :: GetRegexMatchSet)
{-# DEPRECATED grmsRegexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead." #-}

instance Lude.AWSRequest GetRegexMatchSet where
  type Rs GetRegexMatchSet = GetRegexMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRegexMatchSetResponse'
            Lude.<$> (x Lude..?> "RegexMatchSet")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRegexMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetRegexMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRegexMatchSet where
  toJSON GetRegexMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("RegexMatchSetId" Lude..= regexMatchSetId)]
      )

instance Lude.ToPath GetRegexMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRegexMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRegexMatchSetResponse' smart constructor.
data GetRegexMatchSetResponse = GetRegexMatchSetResponse'
  { regexMatchSet ::
      Lude.Maybe RegexMatchSet,
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

-- | Creates a value of 'GetRegexMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'regexMatchSet' - Information about the 'RegexMatchSet' that you specified in the @GetRegexMatchSet@ request. For more information, see 'RegexMatchTuple' .
-- * 'responseStatus' - The response status code.
mkGetRegexMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRegexMatchSetResponse
mkGetRegexMatchSetResponse pResponseStatus_ =
  GetRegexMatchSetResponse'
    { regexMatchSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'RegexMatchSet' that you specified in the @GetRegexMatchSet@ request. For more information, see 'RegexMatchTuple' .
--
-- /Note:/ Consider using 'regexMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grmsrsRegexMatchSet :: Lens.Lens' GetRegexMatchSetResponse (Lude.Maybe RegexMatchSet)
grmsrsRegexMatchSet = Lens.lens (regexMatchSet :: GetRegexMatchSetResponse -> Lude.Maybe RegexMatchSet) (\s a -> s {regexMatchSet = a} :: GetRegexMatchSetResponse)
{-# DEPRECATED grmsrsRegexMatchSet "Use generic-lens or generic-optics with 'regexMatchSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grmsrsResponseStatus :: Lens.Lens' GetRegexMatchSetResponse Lude.Int
grmsrsResponseStatus = Lens.lens (responseStatus :: GetRegexMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRegexMatchSetResponse)
{-# DEPRECATED grmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
