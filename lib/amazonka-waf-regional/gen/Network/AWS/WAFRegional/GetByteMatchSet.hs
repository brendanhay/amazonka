{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'ByteMatchSet' specified by @ByteMatchSetId@ .
module Network.AWS.WAFRegional.GetByteMatchSet
  ( -- * Creating a request
    GetByteMatchSet (..),
    mkGetByteMatchSet,

    -- ** Request lenses
    gbmsByteMatchSetId,

    -- * Destructuring the response
    GetByteMatchSetResponse (..),
    mkGetByteMatchSetResponse,

    -- ** Response lenses
    gbmsrsByteMatchSet,
    gbmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkGetByteMatchSet' smart constructor.
newtype GetByteMatchSet = GetByteMatchSet'
  { -- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to get. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
    byteMatchSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetByteMatchSet' with the minimum fields required to make a request.
--
-- * 'byteMatchSetId' - The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to get. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
mkGetByteMatchSet ::
  -- | 'byteMatchSetId'
  Lude.Text ->
  GetByteMatchSet
mkGetByteMatchSet pByteMatchSetId_ =
  GetByteMatchSet' {byteMatchSetId = pByteMatchSetId_}

-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to get. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmsByteMatchSetId :: Lens.Lens' GetByteMatchSet Lude.Text
gbmsByteMatchSetId = Lens.lens (byteMatchSetId :: GetByteMatchSet -> Lude.Text) (\s a -> s {byteMatchSetId = a} :: GetByteMatchSet)
{-# DEPRECATED gbmsByteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead." #-}

instance Lude.AWSRequest GetByteMatchSet where
  type Rs GetByteMatchSet = GetByteMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetByteMatchSetResponse'
            Lude.<$> (x Lude..?> "ByteMatchSet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetByteMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.GetByteMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetByteMatchSet where
  toJSON GetByteMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ByteMatchSetId" Lude..= byteMatchSetId)]
      )

instance Lude.ToPath GetByteMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GetByteMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetByteMatchSetResponse' smart constructor.
data GetByteMatchSetResponse = GetByteMatchSetResponse'
  { -- | Information about the 'ByteMatchSet' that you specified in the @GetByteMatchSet@ request. For more information, see the following topics:
    --
    --
    --     * 'ByteMatchSet' : Contains @ByteMatchSetId@ , @ByteMatchTuples@ , and @Name@
    --
    --
    --     * @ByteMatchTuples@ : Contains an array of 'ByteMatchTuple' objects. Each @ByteMatchTuple@ object contains 'FieldToMatch' , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    byteMatchSet :: Lude.Maybe ByteMatchSet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetByteMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'byteMatchSet' - Information about the 'ByteMatchSet' that you specified in the @GetByteMatchSet@ request. For more information, see the following topics:
--
--
--     * 'ByteMatchSet' : Contains @ByteMatchSetId@ , @ByteMatchTuples@ , and @Name@
--
--
--     * @ByteMatchTuples@ : Contains an array of 'ByteMatchTuple' objects. Each @ByteMatchTuple@ object contains 'FieldToMatch' , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
-- * 'responseStatus' - The response status code.
mkGetByteMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetByteMatchSetResponse
mkGetByteMatchSetResponse pResponseStatus_ =
  GetByteMatchSetResponse'
    { byteMatchSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'ByteMatchSet' that you specified in the @GetByteMatchSet@ request. For more information, see the following topics:
--
--
--     * 'ByteMatchSet' : Contains @ByteMatchSetId@ , @ByteMatchTuples@ , and @Name@
--
--
--     * @ByteMatchTuples@ : Contains an array of 'ByteMatchTuple' objects. Each @ByteMatchTuple@ object contains 'FieldToMatch' , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'byteMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmsrsByteMatchSet :: Lens.Lens' GetByteMatchSetResponse (Lude.Maybe ByteMatchSet)
gbmsrsByteMatchSet = Lens.lens (byteMatchSet :: GetByteMatchSetResponse -> Lude.Maybe ByteMatchSet) (\s a -> s {byteMatchSet = a} :: GetByteMatchSetResponse)
{-# DEPRECATED gbmsrsByteMatchSet "Use generic-lens or generic-optics with 'byteMatchSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbmsrsResponseStatus :: Lens.Lens' GetByteMatchSetResponse Lude.Int
gbmsrsResponseStatus = Lens.lens (responseStatus :: GetByteMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetByteMatchSetResponse)
{-# DEPRECATED gbmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
