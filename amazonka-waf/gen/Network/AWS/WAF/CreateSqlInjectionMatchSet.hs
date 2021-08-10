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
-- Module      : Network.AWS.WAF.CreateSqlInjectionMatchSet
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
-- Creates a SqlInjectionMatchSet, which you use to allow, block, or count
-- requests that contain snippets of SQL code in a specified part of web
-- requests. AWS WAF searches for character sequences that are likely to be
-- malicious strings.
--
-- To create and configure a @SqlInjectionMatchSet@, perform the following
-- steps:
--
-- 1.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateSqlInjectionMatchSet@ request.
--
-- 2.  Submit a @CreateSqlInjectionMatchSet@ request.
--
-- 3.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateSqlInjectionMatchSet request.
--
-- 4.  Submit an UpdateSqlInjectionMatchSet request to specify the parts of
--     web requests in which you want to allow, block, or count malicious
--     SQL code.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAF.CreateSqlInjectionMatchSet
  ( -- * Creating a Request
    CreateSqlInjectionMatchSet (..),
    newCreateSqlInjectionMatchSet,

    -- * Request Lenses
    createSqlInjectionMatchSet_name,
    createSqlInjectionMatchSet_changeToken,

    -- * Destructuring the Response
    CreateSqlInjectionMatchSetResponse (..),
    newCreateSqlInjectionMatchSetResponse,

    -- * Response Lenses
    createSqlInjectionMatchSetResponse_sqlInjectionMatchSet,
    createSqlInjectionMatchSetResponse_changeToken,
    createSqlInjectionMatchSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | A request to create a SqlInjectionMatchSet.
--
-- /See:/ 'newCreateSqlInjectionMatchSet' smart constructor.
data CreateSqlInjectionMatchSet = CreateSqlInjectionMatchSet'
  { -- | A friendly name or description for the SqlInjectionMatchSet that you\'re
    -- creating. You can\'t change @Name@ after you create the
    -- @SqlInjectionMatchSet@.
    name :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSqlInjectionMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createSqlInjectionMatchSet_name' - A friendly name or description for the SqlInjectionMatchSet that you\'re
-- creating. You can\'t change @Name@ after you create the
-- @SqlInjectionMatchSet@.
--
-- 'changeToken', 'createSqlInjectionMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateSqlInjectionMatchSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  CreateSqlInjectionMatchSet
newCreateSqlInjectionMatchSet pName_ pChangeToken_ =
  CreateSqlInjectionMatchSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description for the SqlInjectionMatchSet that you\'re
-- creating. You can\'t change @Name@ after you create the
-- @SqlInjectionMatchSet@.
createSqlInjectionMatchSet_name :: Lens.Lens' CreateSqlInjectionMatchSet Prelude.Text
createSqlInjectionMatchSet_name = Lens.lens (\CreateSqlInjectionMatchSet' {name} -> name) (\s@CreateSqlInjectionMatchSet' {} a -> s {name = a} :: CreateSqlInjectionMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
createSqlInjectionMatchSet_changeToken :: Lens.Lens' CreateSqlInjectionMatchSet Prelude.Text
createSqlInjectionMatchSet_changeToken = Lens.lens (\CreateSqlInjectionMatchSet' {changeToken} -> changeToken) (\s@CreateSqlInjectionMatchSet' {} a -> s {changeToken = a} :: CreateSqlInjectionMatchSet)

instance Core.AWSRequest CreateSqlInjectionMatchSet where
  type
    AWSResponse CreateSqlInjectionMatchSet =
      CreateSqlInjectionMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSqlInjectionMatchSetResponse'
            Prelude.<$> (x Core..?> "SqlInjectionMatchSet")
            Prelude.<*> (x Core..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSqlInjectionMatchSet

instance Prelude.NFData CreateSqlInjectionMatchSet

instance Core.ToHeaders CreateSqlInjectionMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.CreateSqlInjectionMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSqlInjectionMatchSet where
  toJSON CreateSqlInjectionMatchSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath CreateSqlInjectionMatchSet where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSqlInjectionMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | The response to a @CreateSqlInjectionMatchSet@ request.
--
-- /See:/ 'newCreateSqlInjectionMatchSetResponse' smart constructor.
data CreateSqlInjectionMatchSetResponse = CreateSqlInjectionMatchSetResponse'
  { -- | A SqlInjectionMatchSet.
    sqlInjectionMatchSet :: Prelude.Maybe SqlInjectionMatchSet,
    -- | The @ChangeToken@ that you used to submit the
    -- @CreateSqlInjectionMatchSet@ request. You can also use this value to
    -- query the status of the request. For more information, see
    -- GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSqlInjectionMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sqlInjectionMatchSet', 'createSqlInjectionMatchSetResponse_sqlInjectionMatchSet' - A SqlInjectionMatchSet.
--
-- 'changeToken', 'createSqlInjectionMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the
-- @CreateSqlInjectionMatchSet@ request. You can also use this value to
-- query the status of the request. For more information, see
-- GetChangeTokenStatus.
--
-- 'httpStatus', 'createSqlInjectionMatchSetResponse_httpStatus' - The response's http status code.
newCreateSqlInjectionMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSqlInjectionMatchSetResponse
newCreateSqlInjectionMatchSetResponse pHttpStatus_ =
  CreateSqlInjectionMatchSetResponse'
    { sqlInjectionMatchSet =
        Prelude.Nothing,
      changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A SqlInjectionMatchSet.
createSqlInjectionMatchSetResponse_sqlInjectionMatchSet :: Lens.Lens' CreateSqlInjectionMatchSetResponse (Prelude.Maybe SqlInjectionMatchSet)
createSqlInjectionMatchSetResponse_sqlInjectionMatchSet = Lens.lens (\CreateSqlInjectionMatchSetResponse' {sqlInjectionMatchSet} -> sqlInjectionMatchSet) (\s@CreateSqlInjectionMatchSetResponse' {} a -> s {sqlInjectionMatchSet = a} :: CreateSqlInjectionMatchSetResponse)

-- | The @ChangeToken@ that you used to submit the
-- @CreateSqlInjectionMatchSet@ request. You can also use this value to
-- query the status of the request. For more information, see
-- GetChangeTokenStatus.
createSqlInjectionMatchSetResponse_changeToken :: Lens.Lens' CreateSqlInjectionMatchSetResponse (Prelude.Maybe Prelude.Text)
createSqlInjectionMatchSetResponse_changeToken = Lens.lens (\CreateSqlInjectionMatchSetResponse' {changeToken} -> changeToken) (\s@CreateSqlInjectionMatchSetResponse' {} a -> s {changeToken = a} :: CreateSqlInjectionMatchSetResponse)

-- | The response's http status code.
createSqlInjectionMatchSetResponse_httpStatus :: Lens.Lens' CreateSqlInjectionMatchSetResponse Prelude.Int
createSqlInjectionMatchSetResponse_httpStatus = Lens.lens (\CreateSqlInjectionMatchSetResponse' {httpStatus} -> httpStatus) (\s@CreateSqlInjectionMatchSetResponse' {} a -> s {httpStatus = a} :: CreateSqlInjectionMatchSetResponse)

instance
  Prelude.NFData
    CreateSqlInjectionMatchSetResponse
