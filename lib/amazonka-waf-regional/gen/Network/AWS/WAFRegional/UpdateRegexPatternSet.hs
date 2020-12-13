{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes @RegexPatternString@ objects in a 'RegexPatternSet' . For each @RegexPatternString@ object, you specify the following values:
--
--
--     * Whether to insert or delete the @RegexPatternString@ .
--
--
--     * The regular expression pattern that you want to insert or delete. For more information, see 'RegexPatternSet' .
--
--
-- For example, you can create a @RegexPatternString@ such as @B[a@]dB[o0]t@ . AWS WAF will match this @RegexPatternString@ to:
--
--     * BadBot
--
--
--     * BadB0t
--
--
--     * B@dBot
--
--
--     * B@dB0t
--
--
-- To create and configure a @RegexPatternSet@ , perform the following steps:
--
--     * Create a @RegexPatternSet.@ For more information, see 'CreateRegexPatternSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexPatternSet@ request.
--
--
--     * Submit an @UpdateRegexPatternSet@ request to specify the regular expression pattern that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateRegexPatternSet
  ( -- * Creating a request
    UpdateRegexPatternSet (..),
    mkUpdateRegexPatternSet,

    -- ** Request lenses
    urpsUpdates,
    urpsRegexPatternSetId,
    urpsChangeToken,

    -- * Destructuring the response
    UpdateRegexPatternSetResponse (..),
    mkUpdateRegexPatternSetResponse,

    -- ** Response lenses
    urpsrsChangeToken,
    urpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkUpdateRegexPatternSet' smart constructor.
data UpdateRegexPatternSet = UpdateRegexPatternSet'
  { -- | An array of @RegexPatternSetUpdate@ objects that you want to insert into or delete from a 'RegexPatternSet' .
    updates :: Lude.NonEmpty RegexPatternSetUpdate,
    -- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to update. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
    regexPatternSetId :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRegexPatternSet' with the minimum fields required to make a request.
--
-- * 'updates' - An array of @RegexPatternSetUpdate@ objects that you want to insert into or delete from a 'RegexPatternSet' .
-- * 'regexPatternSetId' - The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to update. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkUpdateRegexPatternSet ::
  -- | 'updates'
  Lude.NonEmpty RegexPatternSetUpdate ->
  -- | 'regexPatternSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  UpdateRegexPatternSet
mkUpdateRegexPatternSet pUpdates_ pRegexPatternSetId_ pChangeToken_ =
  UpdateRegexPatternSet'
    { updates = pUpdates_,
      regexPatternSetId = pRegexPatternSetId_,
      changeToken = pChangeToken_
    }

-- | An array of @RegexPatternSetUpdate@ objects that you want to insert into or delete from a 'RegexPatternSet' .
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsUpdates :: Lens.Lens' UpdateRegexPatternSet (Lude.NonEmpty RegexPatternSetUpdate)
urpsUpdates = Lens.lens (updates :: UpdateRegexPatternSet -> Lude.NonEmpty RegexPatternSetUpdate) (\s a -> s {updates = a} :: UpdateRegexPatternSet)
{-# DEPRECATED urpsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to update. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsRegexPatternSetId :: Lens.Lens' UpdateRegexPatternSet Lude.Text
urpsRegexPatternSetId = Lens.lens (regexPatternSetId :: UpdateRegexPatternSet -> Lude.Text) (\s a -> s {regexPatternSetId = a} :: UpdateRegexPatternSet)
{-# DEPRECATED urpsRegexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsChangeToken :: Lens.Lens' UpdateRegexPatternSet Lude.Text
urpsChangeToken = Lens.lens (changeToken :: UpdateRegexPatternSet -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateRegexPatternSet)
{-# DEPRECATED urpsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest UpdateRegexPatternSet where
  type Rs UpdateRegexPatternSet = UpdateRegexPatternSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRegexPatternSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRegexPatternSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.UpdateRegexPatternSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRegexPatternSet where
  toJSON UpdateRegexPatternSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Updates" Lude..= updates),
            Lude.Just ("RegexPatternSetId" Lude..= regexPatternSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath UpdateRegexPatternSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRegexPatternSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRegexPatternSetResponse' smart constructor.
data UpdateRegexPatternSetResponse = UpdateRegexPatternSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRegexPatternSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateRegexPatternSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRegexPatternSetResponse
mkUpdateRegexPatternSetResponse pResponseStatus_ =
  UpdateRegexPatternSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsrsChangeToken :: Lens.Lens' UpdateRegexPatternSetResponse (Lude.Maybe Lude.Text)
urpsrsChangeToken = Lens.lens (changeToken :: UpdateRegexPatternSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateRegexPatternSetResponse)
{-# DEPRECATED urpsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpsrsResponseStatus :: Lens.Lens' UpdateRegexPatternSetResponse Lude.Int
urpsrsResponseStatus = Lens.lens (responseStatus :: UpdateRegexPatternSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRegexPatternSetResponse)
{-# DEPRECATED urpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
