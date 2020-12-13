{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateSizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'SizeConstraint' objects (filters) in a 'SizeConstraintSet' . For each @SizeConstraint@ object, you specify the following values:
--
--
--     * Whether to insert or delete the object from the array. If you want to change a @SizeConstraintSetUpdate@ object, you delete the existing object and add a new one.
--
--
--     * The part of a web request that you want AWS WAF to evaluate, such as the length of a query string or the length of the @User-Agent@ header.
--
--
--     * Whether to perform any transformations on the request, such as converting it to lowercase, before checking its length. Note that transformations of the request body are not supported because the AWS resource forwards only the first @8192@ bytes of your request to AWS WAF.
-- You can only specify a single type of TextTransformation.
--
--
--     * A @ComparisonOperator@ used for evaluating the selected part of the request against the specified @Size@ , such as equals, greater than, less than, and so on.
--
--
--     * The length, in bytes, that you want AWS WAF to watch for in selected part of the request. The length is computed after applying the transformation.
--
--
-- For example, you can add a @SizeConstraintSetUpdate@ object that matches web requests in which the length of the @User-Agent@ header is greater than 100 bytes. You can then configure AWS WAF to block those requests.
-- To create and configure a @SizeConstraintSet@ , perform the following steps:
--
--     * Create a @SizeConstraintSet.@ For more information, see 'CreateSizeConstraintSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateSizeConstraintSet@ request.
--
--
--     * Submit an @UpdateSizeConstraintSet@ request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateSizeConstraintSet
  ( -- * Creating a request
    UpdateSizeConstraintSet (..),
    mkUpdateSizeConstraintSet,

    -- ** Request lenses
    uscsSizeConstraintSetId,
    uscsUpdates,
    uscsChangeToken,

    -- * Destructuring the response
    UpdateSizeConstraintSetResponse (..),
    mkUpdateSizeConstraintSetResponse,

    -- ** Response lenses
    uscsrsChangeToken,
    uscsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkUpdateSizeConstraintSet' smart constructor.
data UpdateSizeConstraintSet = UpdateSizeConstraintSet'
  { -- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to update. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
    sizeConstraintSetId :: Lude.Text,
    -- | An array of @SizeConstraintSetUpdate@ objects that you want to insert into or delete from a 'SizeConstraintSet' . For more information, see the applicable data types:
    --
    --
    --     * 'SizeConstraintSetUpdate' : Contains @Action@ and @SizeConstraint@
    --
    --
    --     * 'SizeConstraint' : Contains @FieldToMatch@ , @TextTransformation@ , @ComparisonOperator@ , and @Size@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    updates :: Lude.NonEmpty SizeConstraintSetUpdate,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSizeConstraintSet' with the minimum fields required to make a request.
--
-- * 'sizeConstraintSetId' - The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to update. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
-- * 'updates' - An array of @SizeConstraintSetUpdate@ objects that you want to insert into or delete from a 'SizeConstraintSet' . For more information, see the applicable data types:
--
--
--     * 'SizeConstraintSetUpdate' : Contains @Action@ and @SizeConstraint@
--
--
--     * 'SizeConstraint' : Contains @FieldToMatch@ , @TextTransformation@ , @ComparisonOperator@ , and @Size@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkUpdateSizeConstraintSet ::
  -- | 'sizeConstraintSetId'
  Lude.Text ->
  -- | 'updates'
  Lude.NonEmpty SizeConstraintSetUpdate ->
  -- | 'changeToken'
  Lude.Text ->
  UpdateSizeConstraintSet
mkUpdateSizeConstraintSet
  pSizeConstraintSetId_
  pUpdates_
  pChangeToken_ =
    UpdateSizeConstraintSet'
      { sizeConstraintSetId =
          pSizeConstraintSetId_,
        updates = pUpdates_,
        changeToken = pChangeToken_
      }

-- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to update. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- /Note:/ Consider using 'sizeConstraintSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsSizeConstraintSetId :: Lens.Lens' UpdateSizeConstraintSet Lude.Text
uscsSizeConstraintSetId = Lens.lens (sizeConstraintSetId :: UpdateSizeConstraintSet -> Lude.Text) (\s a -> s {sizeConstraintSetId = a} :: UpdateSizeConstraintSet)
{-# DEPRECATED uscsSizeConstraintSetId "Use generic-lens or generic-optics with 'sizeConstraintSetId' instead." #-}

-- | An array of @SizeConstraintSetUpdate@ objects that you want to insert into or delete from a 'SizeConstraintSet' . For more information, see the applicable data types:
--
--
--     * 'SizeConstraintSetUpdate' : Contains @Action@ and @SizeConstraint@
--
--
--     * 'SizeConstraint' : Contains @FieldToMatch@ , @TextTransformation@ , @ComparisonOperator@ , and @Size@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsUpdates :: Lens.Lens' UpdateSizeConstraintSet (Lude.NonEmpty SizeConstraintSetUpdate)
uscsUpdates = Lens.lens (updates :: UpdateSizeConstraintSet -> Lude.NonEmpty SizeConstraintSetUpdate) (\s a -> s {updates = a} :: UpdateSizeConstraintSet)
{-# DEPRECATED uscsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsChangeToken :: Lens.Lens' UpdateSizeConstraintSet Lude.Text
uscsChangeToken = Lens.lens (changeToken :: UpdateSizeConstraintSet -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateSizeConstraintSet)
{-# DEPRECATED uscsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest UpdateSizeConstraintSet where
  type Rs UpdateSizeConstraintSet = UpdateSizeConstraintSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSizeConstraintSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSizeConstraintSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.UpdateSizeConstraintSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSizeConstraintSet where
  toJSON UpdateSizeConstraintSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SizeConstraintSetId" Lude..= sizeConstraintSetId),
            Lude.Just ("Updates" Lude..= updates),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath UpdateSizeConstraintSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSizeConstraintSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSizeConstraintSetResponse' smart constructor.
data UpdateSizeConstraintSetResponse = UpdateSizeConstraintSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSizeConstraintSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateSizeConstraintSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSizeConstraintSetResponse
mkUpdateSizeConstraintSetResponse pResponseStatus_ =
  UpdateSizeConstraintSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsrsChangeToken :: Lens.Lens' UpdateSizeConstraintSetResponse (Lude.Maybe Lude.Text)
uscsrsChangeToken = Lens.lens (changeToken :: UpdateSizeConstraintSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateSizeConstraintSetResponse)
{-# DEPRECATED uscsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscsrsResponseStatus :: Lens.Lens' UpdateSizeConstraintSetResponse Lude.Int
uscsrsResponseStatus = Lens.lens (responseStatus :: UpdateSizeConstraintSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSizeConstraintSetResponse)
{-# DEPRECATED uscsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
