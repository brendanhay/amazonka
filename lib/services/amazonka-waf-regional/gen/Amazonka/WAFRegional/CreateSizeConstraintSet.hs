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
-- Module      : Amazonka.WAFRegional.CreateSizeConstraintSet
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Creates a @SizeConstraintSet@. You then use UpdateSizeConstraintSet to
-- identify the part of a web request that you want AWS WAF to check for
-- length, such as the length of the @User-Agent@ header or the length of
-- the query string. For example, you can create a @SizeConstraintSet@ that
-- matches any requests that have a query string that is longer than 100
-- bytes. You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @SizeConstraintSet@, perform the following
-- steps:
--
-- 1.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateSizeConstraintSet@ request.
--
-- 2.  Submit a @CreateSizeConstraintSet@ request.
--
-- 3.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an @UpdateSizeConstraintSet@ request.
--
-- 4.  Submit an UpdateSizeConstraintSet request to specify the part of the
--     request that you want AWS WAF to inspect (for example, the header or
--     the URI) and the value that you want AWS WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.CreateSizeConstraintSet
  ( -- * Creating a Request
    CreateSizeConstraintSet (..),
    newCreateSizeConstraintSet,

    -- * Request Lenses
    createSizeConstraintSet_name,
    createSizeConstraintSet_changeToken,

    -- * Destructuring the Response
    CreateSizeConstraintSetResponse (..),
    newCreateSizeConstraintSetResponse,

    -- * Response Lenses
    createSizeConstraintSetResponse_sizeConstraintSet,
    createSizeConstraintSetResponse_changeToken,
    createSizeConstraintSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newCreateSizeConstraintSet' smart constructor.
data CreateSizeConstraintSet = CreateSizeConstraintSet'
  { -- | A friendly name or description of the SizeConstraintSet. You can\'t
    -- change @Name@ after you create a @SizeConstraintSet@.
    name :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSizeConstraintSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createSizeConstraintSet_name' - A friendly name or description of the SizeConstraintSet. You can\'t
-- change @Name@ after you create a @SizeConstraintSet@.
--
-- 'changeToken', 'createSizeConstraintSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateSizeConstraintSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  CreateSizeConstraintSet
newCreateSizeConstraintSet pName_ pChangeToken_ =
  CreateSizeConstraintSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description of the SizeConstraintSet. You can\'t
-- change @Name@ after you create a @SizeConstraintSet@.
createSizeConstraintSet_name :: Lens.Lens' CreateSizeConstraintSet Prelude.Text
createSizeConstraintSet_name = Lens.lens (\CreateSizeConstraintSet' {name} -> name) (\s@CreateSizeConstraintSet' {} a -> s {name = a} :: CreateSizeConstraintSet)

-- | The value returned by the most recent call to GetChangeToken.
createSizeConstraintSet_changeToken :: Lens.Lens' CreateSizeConstraintSet Prelude.Text
createSizeConstraintSet_changeToken = Lens.lens (\CreateSizeConstraintSet' {changeToken} -> changeToken) (\s@CreateSizeConstraintSet' {} a -> s {changeToken = a} :: CreateSizeConstraintSet)

instance Core.AWSRequest CreateSizeConstraintSet where
  type
    AWSResponse CreateSizeConstraintSet =
      CreateSizeConstraintSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSizeConstraintSetResponse'
            Prelude.<$> (x Core..?> "SizeConstraintSet")
            Prelude.<*> (x Core..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSizeConstraintSet where
  hashWithSalt _salt CreateSizeConstraintSet' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData CreateSizeConstraintSet where
  rnf CreateSizeConstraintSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf changeToken

instance Core.ToHeaders CreateSizeConstraintSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.CreateSizeConstraintSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSizeConstraintSet where
  toJSON CreateSizeConstraintSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath CreateSizeConstraintSet where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSizeConstraintSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSizeConstraintSetResponse' smart constructor.
data CreateSizeConstraintSetResponse = CreateSizeConstraintSetResponse'
  { -- | A SizeConstraintSet that contains no @SizeConstraint@ objects.
    sizeConstraintSet :: Prelude.Maybe SizeConstraintSet,
    -- | The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSizeConstraintSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeConstraintSet', 'createSizeConstraintSetResponse_sizeConstraintSet' - A SizeConstraintSet that contains no @SizeConstraint@ objects.
--
-- 'changeToken', 'createSizeConstraintSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'createSizeConstraintSetResponse_httpStatus' - The response's http status code.
newCreateSizeConstraintSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSizeConstraintSetResponse
newCreateSizeConstraintSetResponse pHttpStatus_ =
  CreateSizeConstraintSetResponse'
    { sizeConstraintSet =
        Prelude.Nothing,
      changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A SizeConstraintSet that contains no @SizeConstraint@ objects.
createSizeConstraintSetResponse_sizeConstraintSet :: Lens.Lens' CreateSizeConstraintSetResponse (Prelude.Maybe SizeConstraintSet)
createSizeConstraintSetResponse_sizeConstraintSet = Lens.lens (\CreateSizeConstraintSetResponse' {sizeConstraintSet} -> sizeConstraintSet) (\s@CreateSizeConstraintSetResponse' {} a -> s {sizeConstraintSet = a} :: CreateSizeConstraintSetResponse)

-- | The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
createSizeConstraintSetResponse_changeToken :: Lens.Lens' CreateSizeConstraintSetResponse (Prelude.Maybe Prelude.Text)
createSizeConstraintSetResponse_changeToken = Lens.lens (\CreateSizeConstraintSetResponse' {changeToken} -> changeToken) (\s@CreateSizeConstraintSetResponse' {} a -> s {changeToken = a} :: CreateSizeConstraintSetResponse)

-- | The response's http status code.
createSizeConstraintSetResponse_httpStatus :: Lens.Lens' CreateSizeConstraintSetResponse Prelude.Int
createSizeConstraintSetResponse_httpStatus = Lens.lens (\CreateSizeConstraintSetResponse' {httpStatus} -> httpStatus) (\s@CreateSizeConstraintSetResponse' {} a -> s {httpStatus = a} :: CreateSizeConstraintSetResponse)

instance
  Prelude.NFData
    CreateSizeConstraintSetResponse
  where
  rnf CreateSizeConstraintSetResponse' {..} =
    Prelude.rnf sizeConstraintSet
      `Prelude.seq` Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
