{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetSizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'SizeConstraintSet' specified by @SizeConstraintSetId@ .
module Network.AWS.WAF.GetSizeConstraintSet
  ( -- * Creating a request
    GetSizeConstraintSet (..),
    mkGetSizeConstraintSet,

    -- ** Request lenses
    gscsSizeConstraintSetId,

    -- * Destructuring the response
    GetSizeConstraintSetResponse (..),
    mkGetSizeConstraintSetResponse,

    -- ** Response lenses
    gscsrsSizeConstraintSet,
    gscsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkGetSizeConstraintSet' smart constructor.
newtype GetSizeConstraintSet = GetSizeConstraintSet'
  { -- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to get. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
    sizeConstraintSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSizeConstraintSet' with the minimum fields required to make a request.
--
-- * 'sizeConstraintSetId' - The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to get. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
mkGetSizeConstraintSet ::
  -- | 'sizeConstraintSetId'
  Lude.Text ->
  GetSizeConstraintSet
mkGetSizeConstraintSet pSizeConstraintSetId_ =
  GetSizeConstraintSet'
    { sizeConstraintSetId =
        pSizeConstraintSetId_
    }

-- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to get. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- /Note:/ Consider using 'sizeConstraintSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsSizeConstraintSetId :: Lens.Lens' GetSizeConstraintSet Lude.Text
gscsSizeConstraintSetId = Lens.lens (sizeConstraintSetId :: GetSizeConstraintSet -> Lude.Text) (\s a -> s {sizeConstraintSetId = a} :: GetSizeConstraintSet)
{-# DEPRECATED gscsSizeConstraintSetId "Use generic-lens or generic-optics with 'sizeConstraintSetId' instead." #-}

instance Lude.AWSRequest GetSizeConstraintSet where
  type Rs GetSizeConstraintSet = GetSizeConstraintSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSizeConstraintSetResponse'
            Lude.<$> (x Lude..?> "SizeConstraintSet")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSizeConstraintSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetSizeConstraintSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSizeConstraintSet where
  toJSON GetSizeConstraintSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SizeConstraintSetId" Lude..= sizeConstraintSetId)]
      )

instance Lude.ToPath GetSizeConstraintSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSizeConstraintSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSizeConstraintSetResponse' smart constructor.
data GetSizeConstraintSetResponse = GetSizeConstraintSetResponse'
  { -- | Information about the 'SizeConstraintSet' that you specified in the @GetSizeConstraintSet@ request. For more information, see the following topics:
    --
    --
    --     * 'SizeConstraintSet' : Contains @SizeConstraintSetId@ , @SizeConstraints@ , and @Name@
    --
    --
    --     * @SizeConstraints@ : Contains an array of 'SizeConstraint' objects. Each @SizeConstraint@ object contains 'FieldToMatch' , @TextTransformation@ , @ComparisonOperator@ , and @Size@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    sizeConstraintSet :: Lude.Maybe SizeConstraintSet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSizeConstraintSetResponse' with the minimum fields required to make a request.
--
-- * 'sizeConstraintSet' - Information about the 'SizeConstraintSet' that you specified in the @GetSizeConstraintSet@ request. For more information, see the following topics:
--
--
--     * 'SizeConstraintSet' : Contains @SizeConstraintSetId@ , @SizeConstraints@ , and @Name@
--
--
--     * @SizeConstraints@ : Contains an array of 'SizeConstraint' objects. Each @SizeConstraint@ object contains 'FieldToMatch' , @TextTransformation@ , @ComparisonOperator@ , and @Size@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
-- * 'responseStatus' - The response status code.
mkGetSizeConstraintSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSizeConstraintSetResponse
mkGetSizeConstraintSetResponse pResponseStatus_ =
  GetSizeConstraintSetResponse'
    { sizeConstraintSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'SizeConstraintSet' that you specified in the @GetSizeConstraintSet@ request. For more information, see the following topics:
--
--
--     * 'SizeConstraintSet' : Contains @SizeConstraintSetId@ , @SizeConstraints@ , and @Name@
--
--
--     * @SizeConstraints@ : Contains an array of 'SizeConstraint' objects. Each @SizeConstraint@ object contains 'FieldToMatch' , @TextTransformation@ , @ComparisonOperator@ , and @Size@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'sizeConstraintSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrsSizeConstraintSet :: Lens.Lens' GetSizeConstraintSetResponse (Lude.Maybe SizeConstraintSet)
gscsrsSizeConstraintSet = Lens.lens (sizeConstraintSet :: GetSizeConstraintSetResponse -> Lude.Maybe SizeConstraintSet) (\s a -> s {sizeConstraintSet = a} :: GetSizeConstraintSetResponse)
{-# DEPRECATED gscsrsSizeConstraintSet "Use generic-lens or generic-optics with 'sizeConstraintSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrsResponseStatus :: Lens.Lens' GetSizeConstraintSetResponse Lude.Int
gscsrsResponseStatus = Lens.lens (responseStatus :: GetSizeConstraintSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSizeConstraintSetResponse)
{-# DEPRECATED gscsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
