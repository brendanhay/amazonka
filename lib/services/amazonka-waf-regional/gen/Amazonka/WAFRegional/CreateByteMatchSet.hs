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
-- Module      : Amazonka.WAFRegional.CreateByteMatchSet
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
-- Creates a @ByteMatchSet@. You then use UpdateByteMatchSet to identify
-- the part of a web request that you want AWS WAF to inspect, such as the
-- values of the @User-Agent@ header or the query string. For example, you
-- can create a @ByteMatchSet@ that matches any requests with @User-Agent@
-- headers that contain the string @BadBot@. You can then configure AWS WAF
-- to reject those requests.
--
-- To create and configure a @ByteMatchSet@, perform the following steps:
--
-- 1.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateByteMatchSet@ request.
--
-- 2.  Submit a @CreateByteMatchSet@ request.
--
-- 3.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an @UpdateByteMatchSet@ request.
--
-- 4.  Submit an UpdateByteMatchSet request to specify the part of the
--     request that you want AWS WAF to inspect (for example, the header or
--     the URI) and the value that you want AWS WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.CreateByteMatchSet
  ( -- * Creating a Request
    CreateByteMatchSet (..),
    newCreateByteMatchSet,

    -- * Request Lenses
    createByteMatchSet_name,
    createByteMatchSet_changeToken,

    -- * Destructuring the Response
    CreateByteMatchSetResponse (..),
    newCreateByteMatchSetResponse,

    -- * Response Lenses
    createByteMatchSetResponse_byteMatchSet,
    createByteMatchSetResponse_changeToken,
    createByteMatchSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newCreateByteMatchSet' smart constructor.
data CreateByteMatchSet = CreateByteMatchSet'
  { -- | A friendly name or description of the ByteMatchSet. You can\'t change
    -- @Name@ after you create a @ByteMatchSet@.
    name :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateByteMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createByteMatchSet_name' - A friendly name or description of the ByteMatchSet. You can\'t change
-- @Name@ after you create a @ByteMatchSet@.
--
-- 'changeToken', 'createByteMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateByteMatchSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  CreateByteMatchSet
newCreateByteMatchSet pName_ pChangeToken_ =
  CreateByteMatchSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description of the ByteMatchSet. You can\'t change
-- @Name@ after you create a @ByteMatchSet@.
createByteMatchSet_name :: Lens.Lens' CreateByteMatchSet Prelude.Text
createByteMatchSet_name = Lens.lens (\CreateByteMatchSet' {name} -> name) (\s@CreateByteMatchSet' {} a -> s {name = a} :: CreateByteMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
createByteMatchSet_changeToken :: Lens.Lens' CreateByteMatchSet Prelude.Text
createByteMatchSet_changeToken = Lens.lens (\CreateByteMatchSet' {changeToken} -> changeToken) (\s@CreateByteMatchSet' {} a -> s {changeToken = a} :: CreateByteMatchSet)

instance Core.AWSRequest CreateByteMatchSet where
  type
    AWSResponse CreateByteMatchSet =
      CreateByteMatchSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateByteMatchSetResponse'
            Prelude.<$> (x Core..?> "ByteMatchSet")
            Prelude.<*> (x Core..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateByteMatchSet where
  hashWithSalt _salt CreateByteMatchSet' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData CreateByteMatchSet where
  rnf CreateByteMatchSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf changeToken

instance Core.ToHeaders CreateByteMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.CreateByteMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateByteMatchSet where
  toJSON CreateByteMatchSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath CreateByteMatchSet where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateByteMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateByteMatchSetResponse' smart constructor.
data CreateByteMatchSetResponse = CreateByteMatchSetResponse'
  { -- | A ByteMatchSet that contains no @ByteMatchTuple@ objects.
    byteMatchSet :: Prelude.Maybe ByteMatchSet,
    -- | The @ChangeToken@ that you used to submit the @CreateByteMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateByteMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byteMatchSet', 'createByteMatchSetResponse_byteMatchSet' - A ByteMatchSet that contains no @ByteMatchTuple@ objects.
--
-- 'changeToken', 'createByteMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateByteMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'createByteMatchSetResponse_httpStatus' - The response's http status code.
newCreateByteMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateByteMatchSetResponse
newCreateByteMatchSetResponse pHttpStatus_ =
  CreateByteMatchSetResponse'
    { byteMatchSet =
        Prelude.Nothing,
      changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A ByteMatchSet that contains no @ByteMatchTuple@ objects.
createByteMatchSetResponse_byteMatchSet :: Lens.Lens' CreateByteMatchSetResponse (Prelude.Maybe ByteMatchSet)
createByteMatchSetResponse_byteMatchSet = Lens.lens (\CreateByteMatchSetResponse' {byteMatchSet} -> byteMatchSet) (\s@CreateByteMatchSetResponse' {} a -> s {byteMatchSet = a} :: CreateByteMatchSetResponse)

-- | The @ChangeToken@ that you used to submit the @CreateByteMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
createByteMatchSetResponse_changeToken :: Lens.Lens' CreateByteMatchSetResponse (Prelude.Maybe Prelude.Text)
createByteMatchSetResponse_changeToken = Lens.lens (\CreateByteMatchSetResponse' {changeToken} -> changeToken) (\s@CreateByteMatchSetResponse' {} a -> s {changeToken = a} :: CreateByteMatchSetResponse)

-- | The response's http status code.
createByteMatchSetResponse_httpStatus :: Lens.Lens' CreateByteMatchSetResponse Prelude.Int
createByteMatchSetResponse_httpStatus = Lens.lens (\CreateByteMatchSetResponse' {httpStatus} -> httpStatus) (\s@CreateByteMatchSetResponse' {} a -> s {httpStatus = a} :: CreateByteMatchSetResponse)

instance Prelude.NFData CreateByteMatchSetResponse where
  rnf CreateByteMatchSetResponse' {..} =
    Prelude.rnf byteMatchSet
      `Prelude.seq` Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
