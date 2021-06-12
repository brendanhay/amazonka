{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateRegexMatchSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Creates a RegexMatchSet. You then use UpdateRegexMatchSet to identify
-- the part of a web request that you want AWS WAF to inspect, such as the
-- values of the @User-Agent@ header or the query string. For example, you
-- can create a @RegexMatchSet@ that contains a @RegexMatchTuple@ that
-- looks for any requests with @User-Agent@ headers that match a
-- @RegexPatternSet@ with pattern @B[a\@]dB[o0]t@. You can then configure
-- AWS WAF to reject those requests.
--
-- To create and configure a @RegexMatchSet@, perform the following steps:
--
-- 1.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateRegexMatchSet@ request.
--
-- 2.  Submit a @CreateRegexMatchSet@ request.
--
-- 3.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an @UpdateRegexMatchSet@ request.
--
-- 4.  Submit an UpdateRegexMatchSet request to specify the part of the
--     request that you want AWS WAF to inspect (for example, the header or
--     the URI) and the value, using a @RegexPatternSet@, that you want AWS
--     WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAF.CreateRegexMatchSet
  ( -- * Creating a Request
    CreateRegexMatchSet (..),
    newCreateRegexMatchSet,

    -- * Request Lenses
    createRegexMatchSet_name,
    createRegexMatchSet_changeToken,

    -- * Destructuring the Response
    CreateRegexMatchSetResponse (..),
    newCreateRegexMatchSetResponse,

    -- * Response Lenses
    createRegexMatchSetResponse_regexMatchSet,
    createRegexMatchSetResponse_changeToken,
    createRegexMatchSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newCreateRegexMatchSet' smart constructor.
data CreateRegexMatchSet = CreateRegexMatchSet'
  { -- | A friendly name or description of the RegexMatchSet. You can\'t change
    -- @Name@ after you create a @RegexMatchSet@.
    name :: Core.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRegexMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createRegexMatchSet_name' - A friendly name or description of the RegexMatchSet. You can\'t change
-- @Name@ after you create a @RegexMatchSet@.
--
-- 'changeToken', 'createRegexMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateRegexMatchSet ::
  -- | 'name'
  Core.Text ->
  -- | 'changeToken'
  Core.Text ->
  CreateRegexMatchSet
newCreateRegexMatchSet pName_ pChangeToken_ =
  CreateRegexMatchSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description of the RegexMatchSet. You can\'t change
-- @Name@ after you create a @RegexMatchSet@.
createRegexMatchSet_name :: Lens.Lens' CreateRegexMatchSet Core.Text
createRegexMatchSet_name = Lens.lens (\CreateRegexMatchSet' {name} -> name) (\s@CreateRegexMatchSet' {} a -> s {name = a} :: CreateRegexMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
createRegexMatchSet_changeToken :: Lens.Lens' CreateRegexMatchSet Core.Text
createRegexMatchSet_changeToken = Lens.lens (\CreateRegexMatchSet' {changeToken} -> changeToken) (\s@CreateRegexMatchSet' {} a -> s {changeToken = a} :: CreateRegexMatchSet)

instance Core.AWSRequest CreateRegexMatchSet where
  type
    AWSResponse CreateRegexMatchSet =
      CreateRegexMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRegexMatchSetResponse'
            Core.<$> (x Core..?> "RegexMatchSet")
            Core.<*> (x Core..?> "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRegexMatchSet

instance Core.NFData CreateRegexMatchSet

instance Core.ToHeaders CreateRegexMatchSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.CreateRegexMatchSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRegexMatchSet where
  toJSON CreateRegexMatchSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath CreateRegexMatchSet where
  toPath = Core.const "/"

instance Core.ToQuery CreateRegexMatchSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRegexMatchSetResponse' smart constructor.
data CreateRegexMatchSetResponse = CreateRegexMatchSetResponse'
  { -- | A RegexMatchSet that contains no @RegexMatchTuple@ objects.
    regexMatchSet :: Core.Maybe RegexMatchSet,
    -- | The @ChangeToken@ that you used to submit the @CreateRegexMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRegexMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexMatchSet', 'createRegexMatchSetResponse_regexMatchSet' - A RegexMatchSet that contains no @RegexMatchTuple@ objects.
--
-- 'changeToken', 'createRegexMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateRegexMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'createRegexMatchSetResponse_httpStatus' - The response's http status code.
newCreateRegexMatchSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRegexMatchSetResponse
newCreateRegexMatchSetResponse pHttpStatus_ =
  CreateRegexMatchSetResponse'
    { regexMatchSet =
        Core.Nothing,
      changeToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A RegexMatchSet that contains no @RegexMatchTuple@ objects.
createRegexMatchSetResponse_regexMatchSet :: Lens.Lens' CreateRegexMatchSetResponse (Core.Maybe RegexMatchSet)
createRegexMatchSetResponse_regexMatchSet = Lens.lens (\CreateRegexMatchSetResponse' {regexMatchSet} -> regexMatchSet) (\s@CreateRegexMatchSetResponse' {} a -> s {regexMatchSet = a} :: CreateRegexMatchSetResponse)

-- | The @ChangeToken@ that you used to submit the @CreateRegexMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
createRegexMatchSetResponse_changeToken :: Lens.Lens' CreateRegexMatchSetResponse (Core.Maybe Core.Text)
createRegexMatchSetResponse_changeToken = Lens.lens (\CreateRegexMatchSetResponse' {changeToken} -> changeToken) (\s@CreateRegexMatchSetResponse' {} a -> s {changeToken = a} :: CreateRegexMatchSetResponse)

-- | The response's http status code.
createRegexMatchSetResponse_httpStatus :: Lens.Lens' CreateRegexMatchSetResponse Core.Int
createRegexMatchSetResponse_httpStatus = Lens.lens (\CreateRegexMatchSetResponse' {httpStatus} -> httpStatus) (\s@CreateRegexMatchSetResponse' {} a -> s {httpStatus = a} :: CreateRegexMatchSetResponse)

instance Core.NFData CreateRegexMatchSetResponse
