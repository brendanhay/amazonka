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
-- Module      : Network.AWS.WAFRegional.CreateRegexPatternSet
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
-- Creates a @RegexPatternSet@. You then use UpdateRegexPatternSet to
-- specify the regular expression (regex) pattern that you want AWS WAF to
-- search for, such as @B[a\@]dB[o0]t@. You can then configure AWS WAF to
-- reject those requests.
--
-- To create and configure a @RegexPatternSet@, perform the following
-- steps:
--
-- 1.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateRegexPatternSet@ request.
--
-- 2.  Submit a @CreateRegexPatternSet@ request.
--
-- 3.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an @UpdateRegexPatternSet@ request.
--
-- 4.  Submit an UpdateRegexPatternSet request to specify the string that
--     you want AWS WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAFRegional.CreateRegexPatternSet
  ( -- * Creating a Request
    CreateRegexPatternSet (..),
    newCreateRegexPatternSet,

    -- * Request Lenses
    createRegexPatternSet_name,
    createRegexPatternSet_changeToken,

    -- * Destructuring the Response
    CreateRegexPatternSetResponse (..),
    newCreateRegexPatternSetResponse,

    -- * Response Lenses
    createRegexPatternSetResponse_regexPatternSet,
    createRegexPatternSetResponse_changeToken,
    createRegexPatternSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newCreateRegexPatternSet' smart constructor.
data CreateRegexPatternSet = CreateRegexPatternSet'
  { -- | A friendly name or description of the RegexPatternSet. You can\'t change
    -- @Name@ after you create a @RegexPatternSet@.
    name :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRegexPatternSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createRegexPatternSet_name' - A friendly name or description of the RegexPatternSet. You can\'t change
-- @Name@ after you create a @RegexPatternSet@.
--
-- 'changeToken', 'createRegexPatternSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateRegexPatternSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  CreateRegexPatternSet
newCreateRegexPatternSet pName_ pChangeToken_ =
  CreateRegexPatternSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description of the RegexPatternSet. You can\'t change
-- @Name@ after you create a @RegexPatternSet@.
createRegexPatternSet_name :: Lens.Lens' CreateRegexPatternSet Prelude.Text
createRegexPatternSet_name = Lens.lens (\CreateRegexPatternSet' {name} -> name) (\s@CreateRegexPatternSet' {} a -> s {name = a} :: CreateRegexPatternSet)

-- | The value returned by the most recent call to GetChangeToken.
createRegexPatternSet_changeToken :: Lens.Lens' CreateRegexPatternSet Prelude.Text
createRegexPatternSet_changeToken = Lens.lens (\CreateRegexPatternSet' {changeToken} -> changeToken) (\s@CreateRegexPatternSet' {} a -> s {changeToken = a} :: CreateRegexPatternSet)

instance Core.AWSRequest CreateRegexPatternSet where
  type
    AWSResponse CreateRegexPatternSet =
      CreateRegexPatternSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRegexPatternSetResponse'
            Prelude.<$> (x Core..?> "RegexPatternSet")
            Prelude.<*> (x Core..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRegexPatternSet

instance Prelude.NFData CreateRegexPatternSet

instance Core.ToHeaders CreateRegexPatternSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.CreateRegexPatternSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRegexPatternSet where
  toJSON CreateRegexPatternSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath CreateRegexPatternSet where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateRegexPatternSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRegexPatternSetResponse' smart constructor.
data CreateRegexPatternSetResponse = CreateRegexPatternSetResponse'
  { -- | A RegexPatternSet that contains no objects.
    regexPatternSet :: Prelude.Maybe RegexPatternSet,
    -- | The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRegexPatternSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexPatternSet', 'createRegexPatternSetResponse_regexPatternSet' - A RegexPatternSet that contains no objects.
--
-- 'changeToken', 'createRegexPatternSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'createRegexPatternSetResponse_httpStatus' - The response's http status code.
newCreateRegexPatternSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRegexPatternSetResponse
newCreateRegexPatternSetResponse pHttpStatus_ =
  CreateRegexPatternSetResponse'
    { regexPatternSet =
        Prelude.Nothing,
      changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A RegexPatternSet that contains no objects.
createRegexPatternSetResponse_regexPatternSet :: Lens.Lens' CreateRegexPatternSetResponse (Prelude.Maybe RegexPatternSet)
createRegexPatternSetResponse_regexPatternSet = Lens.lens (\CreateRegexPatternSetResponse' {regexPatternSet} -> regexPatternSet) (\s@CreateRegexPatternSetResponse' {} a -> s {regexPatternSet = a} :: CreateRegexPatternSetResponse)

-- | The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
createRegexPatternSetResponse_changeToken :: Lens.Lens' CreateRegexPatternSetResponse (Prelude.Maybe Prelude.Text)
createRegexPatternSetResponse_changeToken = Lens.lens (\CreateRegexPatternSetResponse' {changeToken} -> changeToken) (\s@CreateRegexPatternSetResponse' {} a -> s {changeToken = a} :: CreateRegexPatternSetResponse)

-- | The response's http status code.
createRegexPatternSetResponse_httpStatus :: Lens.Lens' CreateRegexPatternSetResponse Prelude.Int
createRegexPatternSetResponse_httpStatus = Lens.lens (\CreateRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@CreateRegexPatternSetResponse' {} a -> s {httpStatus = a} :: CreateRegexPatternSetResponse)

instance Prelude.NFData CreateRegexPatternSetResponse
