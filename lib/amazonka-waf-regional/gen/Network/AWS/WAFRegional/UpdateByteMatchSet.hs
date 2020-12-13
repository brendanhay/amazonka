{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'ByteMatchTuple' objects (filters) in a 'ByteMatchSet' . For each @ByteMatchTuple@ object, you specify the following values:
--
--
--     * Whether to insert or delete the object from the array. If you want to change a @ByteMatchSetUpdate@ object, you delete the existing object and add a new one.
--
--
--     * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header.
--
--
--     * The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to look for. For more information, including how you specify the values for the AWS WAF API and the AWS CLI or SDKs, see @TargetString@ in the 'ByteMatchTuple' data type.
--
--
--     * Where to look, such as at the beginning or the end of a query string.
--
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
-- For example, you can add a @ByteMatchSetUpdate@ object that matches web requests in which @User-Agent@ headers contain the string @BadBot@ . You can then configure AWS WAF to block those requests.
-- To create and configure a @ByteMatchSet@ , perform the following steps:
--
--     * Create a @ByteMatchSet.@ For more information, see 'CreateByteMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateByteMatchSet@ request.
--
--
--     * Submit an @UpdateByteMatchSet@ request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateByteMatchSet
  ( -- * Creating a request
    UpdateByteMatchSet (..),
    mkUpdateByteMatchSet,

    -- ** Request lenses
    ubmsByteMatchSetId,
    ubmsUpdates,
    ubmsChangeToken,

    -- * Destructuring the response
    UpdateByteMatchSetResponse (..),
    mkUpdateByteMatchSetResponse,

    -- ** Response lenses
    ubmsrsChangeToken,
    ubmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkUpdateByteMatchSet' smart constructor.
data UpdateByteMatchSet = UpdateByteMatchSet'
  { -- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to update. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
    byteMatchSetId :: Lude.Text,
    -- | An array of @ByteMatchSetUpdate@ objects that you want to insert into or delete from a 'ByteMatchSet' . For more information, see the applicable data types:
    --
    --
    --     * 'ByteMatchSetUpdate' : Contains @Action@ and @ByteMatchTuple@
    --
    --
    --     * 'ByteMatchTuple' : Contains @FieldToMatch@ , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    updates :: Lude.NonEmpty ByteMatchSetUpdate,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateByteMatchSet' with the minimum fields required to make a request.
--
-- * 'byteMatchSetId' - The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to update. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
-- * 'updates' - An array of @ByteMatchSetUpdate@ objects that you want to insert into or delete from a 'ByteMatchSet' . For more information, see the applicable data types:
--
--
--     * 'ByteMatchSetUpdate' : Contains @Action@ and @ByteMatchTuple@
--
--
--     * 'ByteMatchTuple' : Contains @FieldToMatch@ , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkUpdateByteMatchSet ::
  -- | 'byteMatchSetId'
  Lude.Text ->
  -- | 'updates'
  Lude.NonEmpty ByteMatchSetUpdate ->
  -- | 'changeToken'
  Lude.Text ->
  UpdateByteMatchSet
mkUpdateByteMatchSet pByteMatchSetId_ pUpdates_ pChangeToken_ =
  UpdateByteMatchSet'
    { byteMatchSetId = pByteMatchSetId_,
      updates = pUpdates_,
      changeToken = pChangeToken_
    }

-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to update. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsByteMatchSetId :: Lens.Lens' UpdateByteMatchSet Lude.Text
ubmsByteMatchSetId = Lens.lens (byteMatchSetId :: UpdateByteMatchSet -> Lude.Text) (\s a -> s {byteMatchSetId = a} :: UpdateByteMatchSet)
{-# DEPRECATED ubmsByteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead." #-}

-- | An array of @ByteMatchSetUpdate@ objects that you want to insert into or delete from a 'ByteMatchSet' . For more information, see the applicable data types:
--
--
--     * 'ByteMatchSetUpdate' : Contains @Action@ and @ByteMatchTuple@
--
--
--     * 'ByteMatchTuple' : Contains @FieldToMatch@ , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsUpdates :: Lens.Lens' UpdateByteMatchSet (Lude.NonEmpty ByteMatchSetUpdate)
ubmsUpdates = Lens.lens (updates :: UpdateByteMatchSet -> Lude.NonEmpty ByteMatchSetUpdate) (\s a -> s {updates = a} :: UpdateByteMatchSet)
{-# DEPRECATED ubmsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsChangeToken :: Lens.Lens' UpdateByteMatchSet Lude.Text
ubmsChangeToken = Lens.lens (changeToken :: UpdateByteMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateByteMatchSet)
{-# DEPRECATED ubmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest UpdateByteMatchSet where
  type Rs UpdateByteMatchSet = UpdateByteMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateByteMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateByteMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.UpdateByteMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateByteMatchSet where
  toJSON UpdateByteMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ByteMatchSetId" Lude..= byteMatchSetId),
            Lude.Just ("Updates" Lude..= updates),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath UpdateByteMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateByteMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateByteMatchSetResponse' smart constructor.
data UpdateByteMatchSetResponse = UpdateByteMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateByteMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateByteMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateByteMatchSetResponse
mkUpdateByteMatchSetResponse pResponseStatus_ =
  UpdateByteMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsrsChangeToken :: Lens.Lens' UpdateByteMatchSetResponse (Lude.Maybe Lude.Text)
ubmsrsChangeToken = Lens.lens (changeToken :: UpdateByteMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateByteMatchSetResponse)
{-# DEPRECATED ubmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubmsrsResponseStatus :: Lens.Lens' UpdateByteMatchSetResponse Lude.Int
ubmsrsResponseStatus = Lens.lens (responseStatus :: UpdateByteMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateByteMatchSetResponse)
{-# DEPRECATED ubmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
