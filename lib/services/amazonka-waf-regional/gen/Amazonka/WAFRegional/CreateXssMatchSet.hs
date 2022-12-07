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
-- Module      : Amazonka.WAFRegional.CreateXssMatchSet
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
-- Creates an XssMatchSet, which you use to allow, block, or count requests
-- that contain cross-site scripting attacks in the specified part of web
-- requests. AWS WAF searches for character sequences that are likely to be
-- malicious strings.
--
-- To create and configure an @XssMatchSet@, perform the following steps:
--
-- 1.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateXssMatchSet@ request.
--
-- 2.  Submit a @CreateXssMatchSet@ request.
--
-- 3.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateXssMatchSet request.
--
-- 4.  Submit an UpdateXssMatchSet request to specify the parts of web
--     requests in which you want to allow, block, or count cross-site
--     scripting attacks.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.CreateXssMatchSet
  ( -- * Creating a Request
    CreateXssMatchSet (..),
    newCreateXssMatchSet,

    -- * Request Lenses
    createXssMatchSet_name,
    createXssMatchSet_changeToken,

    -- * Destructuring the Response
    CreateXssMatchSetResponse (..),
    newCreateXssMatchSetResponse,

    -- * Response Lenses
    createXssMatchSetResponse_xssMatchSet,
    createXssMatchSetResponse_changeToken,
    createXssMatchSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | A request to create an XssMatchSet.
--
-- /See:/ 'newCreateXssMatchSet' smart constructor.
data CreateXssMatchSet = CreateXssMatchSet'
  { -- | A friendly name or description for the XssMatchSet that you\'re
    -- creating. You can\'t change @Name@ after you create the @XssMatchSet@.
    name :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateXssMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createXssMatchSet_name' - A friendly name or description for the XssMatchSet that you\'re
-- creating. You can\'t change @Name@ after you create the @XssMatchSet@.
--
-- 'changeToken', 'createXssMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateXssMatchSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  CreateXssMatchSet
newCreateXssMatchSet pName_ pChangeToken_ =
  CreateXssMatchSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description for the XssMatchSet that you\'re
-- creating. You can\'t change @Name@ after you create the @XssMatchSet@.
createXssMatchSet_name :: Lens.Lens' CreateXssMatchSet Prelude.Text
createXssMatchSet_name = Lens.lens (\CreateXssMatchSet' {name} -> name) (\s@CreateXssMatchSet' {} a -> s {name = a} :: CreateXssMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
createXssMatchSet_changeToken :: Lens.Lens' CreateXssMatchSet Prelude.Text
createXssMatchSet_changeToken = Lens.lens (\CreateXssMatchSet' {changeToken} -> changeToken) (\s@CreateXssMatchSet' {} a -> s {changeToken = a} :: CreateXssMatchSet)

instance Core.AWSRequest CreateXssMatchSet where
  type
    AWSResponse CreateXssMatchSet =
      CreateXssMatchSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateXssMatchSetResponse'
            Prelude.<$> (x Data..?> "XssMatchSet")
            Prelude.<*> (x Data..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateXssMatchSet where
  hashWithSalt _salt CreateXssMatchSet' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData CreateXssMatchSet where
  rnf CreateXssMatchSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf changeToken

instance Data.ToHeaders CreateXssMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.CreateXssMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateXssMatchSet where
  toJSON CreateXssMatchSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ChangeToken" Data..= changeToken)
          ]
      )

instance Data.ToPath CreateXssMatchSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateXssMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | The response to a @CreateXssMatchSet@ request.
--
-- /See:/ 'newCreateXssMatchSetResponse' smart constructor.
data CreateXssMatchSetResponse = CreateXssMatchSetResponse'
  { -- | An XssMatchSet.
    xssMatchSet :: Prelude.Maybe XssMatchSet,
    -- | The @ChangeToken@ that you used to submit the @CreateXssMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateXssMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xssMatchSet', 'createXssMatchSetResponse_xssMatchSet' - An XssMatchSet.
--
-- 'changeToken', 'createXssMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateXssMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'createXssMatchSetResponse_httpStatus' - The response's http status code.
newCreateXssMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateXssMatchSetResponse
newCreateXssMatchSetResponse pHttpStatus_ =
  CreateXssMatchSetResponse'
    { xssMatchSet =
        Prelude.Nothing,
      changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An XssMatchSet.
createXssMatchSetResponse_xssMatchSet :: Lens.Lens' CreateXssMatchSetResponse (Prelude.Maybe XssMatchSet)
createXssMatchSetResponse_xssMatchSet = Lens.lens (\CreateXssMatchSetResponse' {xssMatchSet} -> xssMatchSet) (\s@CreateXssMatchSetResponse' {} a -> s {xssMatchSet = a} :: CreateXssMatchSetResponse)

-- | The @ChangeToken@ that you used to submit the @CreateXssMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
createXssMatchSetResponse_changeToken :: Lens.Lens' CreateXssMatchSetResponse (Prelude.Maybe Prelude.Text)
createXssMatchSetResponse_changeToken = Lens.lens (\CreateXssMatchSetResponse' {changeToken} -> changeToken) (\s@CreateXssMatchSetResponse' {} a -> s {changeToken = a} :: CreateXssMatchSetResponse)

-- | The response's http status code.
createXssMatchSetResponse_httpStatus :: Lens.Lens' CreateXssMatchSetResponse Prelude.Int
createXssMatchSetResponse_httpStatus = Lens.lens (\CreateXssMatchSetResponse' {httpStatus} -> httpStatus) (\s@CreateXssMatchSetResponse' {} a -> s {httpStatus = a} :: CreateXssMatchSetResponse)

instance Prelude.NFData CreateXssMatchSetResponse where
  rnf CreateXssMatchSetResponse' {..} =
    Prelude.rnf xssMatchSet
      `Prelude.seq` Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
