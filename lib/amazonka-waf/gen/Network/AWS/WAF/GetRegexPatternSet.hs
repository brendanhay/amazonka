{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RegexPatternSet' specified by @RegexPatternSetId@ .
module Network.AWS.WAF.GetRegexPatternSet
  ( -- * Creating a request
    GetRegexPatternSet (..),
    mkGetRegexPatternSet,

    -- ** Request lenses
    grpsRegexPatternSetId,

    -- * Destructuring the response
    GetRegexPatternSetResponse (..),
    mkGetRegexPatternSetResponse,

    -- ** Response lenses
    grpsrsRegexPatternSet,
    grpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkGetRegexPatternSet' smart constructor.
newtype GetRegexPatternSet = GetRegexPatternSet'
  { regexPatternSetId ::
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

-- | Creates a value of 'GetRegexPatternSet' with the minimum fields required to make a request.
--
-- * 'regexPatternSetId' - The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to get. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
mkGetRegexPatternSet ::
  -- | 'regexPatternSetId'
  Lude.Text ->
  GetRegexPatternSet
mkGetRegexPatternSet pRegexPatternSetId_ =
  GetRegexPatternSet' {regexPatternSetId = pRegexPatternSetId_}

-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to get. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpsRegexPatternSetId :: Lens.Lens' GetRegexPatternSet Lude.Text
grpsRegexPatternSetId = Lens.lens (regexPatternSetId :: GetRegexPatternSet -> Lude.Text) (\s a -> s {regexPatternSetId = a} :: GetRegexPatternSet)
{-# DEPRECATED grpsRegexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead." #-}

instance Lude.AWSRequest GetRegexPatternSet where
  type Rs GetRegexPatternSet = GetRegexPatternSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRegexPatternSetResponse'
            Lude.<$> (x Lude..?> "RegexPatternSet")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRegexPatternSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetRegexPatternSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRegexPatternSet where
  toJSON GetRegexPatternSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("RegexPatternSetId" Lude..= regexPatternSetId)]
      )

instance Lude.ToPath GetRegexPatternSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRegexPatternSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRegexPatternSetResponse' smart constructor.
data GetRegexPatternSetResponse = GetRegexPatternSetResponse'
  { regexPatternSet ::
      Lude.Maybe RegexPatternSet,
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

-- | Creates a value of 'GetRegexPatternSetResponse' with the minimum fields required to make a request.
--
-- * 'regexPatternSet' - Information about the 'RegexPatternSet' that you specified in the @GetRegexPatternSet@ request, including the identifier of the pattern set and the regular expression patterns you want AWS WAF to search for.
-- * 'responseStatus' - The response status code.
mkGetRegexPatternSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRegexPatternSetResponse
mkGetRegexPatternSetResponse pResponseStatus_ =
  GetRegexPatternSetResponse'
    { regexPatternSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'RegexPatternSet' that you specified in the @GetRegexPatternSet@ request, including the identifier of the pattern set and the regular expression patterns you want AWS WAF to search for.
--
-- /Note:/ Consider using 'regexPatternSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpsrsRegexPatternSet :: Lens.Lens' GetRegexPatternSetResponse (Lude.Maybe RegexPatternSet)
grpsrsRegexPatternSet = Lens.lens (regexPatternSet :: GetRegexPatternSetResponse -> Lude.Maybe RegexPatternSet) (\s a -> s {regexPatternSet = a} :: GetRegexPatternSetResponse)
{-# DEPRECATED grpsrsRegexPatternSet "Use generic-lens or generic-optics with 'regexPatternSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpsrsResponseStatus :: Lens.Lens' GetRegexPatternSetResponse Lude.Int
grpsrsResponseStatus = Lens.lens (responseStatus :: GetRegexPatternSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRegexPatternSetResponse)
{-# DEPRECATED grpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
