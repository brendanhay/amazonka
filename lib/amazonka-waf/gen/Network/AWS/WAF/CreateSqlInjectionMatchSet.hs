{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'SqlInjectionMatchSet' , which you use to allow, block, or count requests that contain snippets of SQL code in a specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.
--
-- To create and configure a @SqlInjectionMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateSqlInjectionMatchSet@ request.
--
--
--     * Submit a @CreateSqlInjectionMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateSqlInjectionMatchSet' request.
--
--
--     * Submit an 'UpdateSqlInjectionMatchSet' request to specify the parts of web requests in which you want to allow, block, or count malicious SQL code.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateSqlInjectionMatchSet
  ( -- * Creating a request
    CreateSqlInjectionMatchSet (..),
    mkCreateSqlInjectionMatchSet,

    -- ** Request lenses
    csimsName,
    csimsChangeToken,

    -- * Destructuring the response
    CreateSqlInjectionMatchSetResponse (..),
    mkCreateSqlInjectionMatchSetResponse,

    -- ** Response lenses
    csimsrsSqlInjectionMatchSet,
    csimsrsChangeToken,
    csimsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | A request to create a 'SqlInjectionMatchSet' .
--
-- /See:/ 'mkCreateSqlInjectionMatchSet' smart constructor.
data CreateSqlInjectionMatchSet = CreateSqlInjectionMatchSet'
  { -- | A friendly name or description for the 'SqlInjectionMatchSet' that you're creating. You can't change @Name@ after you create the @SqlInjectionMatchSet@ .
    name :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description for the 'SqlInjectionMatchSet' that you're creating. You can't change @Name@ after you create the @SqlInjectionMatchSet@ .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkCreateSqlInjectionMatchSet ::
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateSqlInjectionMatchSet
mkCreateSqlInjectionMatchSet pName_ pChangeToken_ =
  CreateSqlInjectionMatchSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description for the 'SqlInjectionMatchSet' that you're creating. You can't change @Name@ after you create the @SqlInjectionMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsName :: Lens.Lens' CreateSqlInjectionMatchSet Lude.Text
csimsName = Lens.lens (name :: CreateSqlInjectionMatchSet -> Lude.Text) (\s a -> s {name = a} :: CreateSqlInjectionMatchSet)
{-# DEPRECATED csimsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsChangeToken :: Lens.Lens' CreateSqlInjectionMatchSet Lude.Text
csimsChangeToken = Lens.lens (changeToken :: CreateSqlInjectionMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: CreateSqlInjectionMatchSet)
{-# DEPRECATED csimsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateSqlInjectionMatchSet where
  type
    Rs CreateSqlInjectionMatchSet =
      CreateSqlInjectionMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSqlInjectionMatchSetResponse'
            Lude.<$> (x Lude..?> "SqlInjectionMatchSet")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSqlInjectionMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.CreateSqlInjectionMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSqlInjectionMatchSet where
  toJSON CreateSqlInjectionMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateSqlInjectionMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSqlInjectionMatchSet where
  toQuery = Lude.const Lude.mempty

-- | The response to a @CreateSqlInjectionMatchSet@ request.
--
-- /See:/ 'mkCreateSqlInjectionMatchSetResponse' smart constructor.
data CreateSqlInjectionMatchSetResponse = CreateSqlInjectionMatchSetResponse'
  { -- | A 'SqlInjectionMatchSet' .
    sqlInjectionMatchSet :: Lude.Maybe SqlInjectionMatchSet,
    -- | The @ChangeToken@ that you used to submit the @CreateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSqlInjectionMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'sqlInjectionMatchSet' - A 'SqlInjectionMatchSet' .
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkCreateSqlInjectionMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSqlInjectionMatchSetResponse
mkCreateSqlInjectionMatchSetResponse pResponseStatus_ =
  CreateSqlInjectionMatchSetResponse'
    { sqlInjectionMatchSet =
        Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'SqlInjectionMatchSet' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsrsSqlInjectionMatchSet :: Lens.Lens' CreateSqlInjectionMatchSetResponse (Lude.Maybe SqlInjectionMatchSet)
csimsrsSqlInjectionMatchSet = Lens.lens (sqlInjectionMatchSet :: CreateSqlInjectionMatchSetResponse -> Lude.Maybe SqlInjectionMatchSet) (\s a -> s {sqlInjectionMatchSet = a} :: CreateSqlInjectionMatchSetResponse)
{-# DEPRECATED csimsrsSqlInjectionMatchSet "Use generic-lens or generic-optics with 'sqlInjectionMatchSet' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsrsChangeToken :: Lens.Lens' CreateSqlInjectionMatchSetResponse (Lude.Maybe Lude.Text)
csimsrsChangeToken = Lens.lens (changeToken :: CreateSqlInjectionMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateSqlInjectionMatchSetResponse)
{-# DEPRECATED csimsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csimsrsResponseStatus :: Lens.Lens' CreateSqlInjectionMatchSetResponse Lude.Int
csimsrsResponseStatus = Lens.lens (responseStatus :: CreateSqlInjectionMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSqlInjectionMatchSetResponse)
{-# DEPRECATED csimsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
