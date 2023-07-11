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
-- Module      : Amazonka.WAFRegional.CreateWebACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- Creates a @WebACL@, which contains the @Rules@ that identify the
-- CloudFront web requests that you want to allow, block, or count. AWS WAF
-- evaluates @Rules@ in order based on the value of @Priority@ for each
-- @Rule@.
--
-- You also specify a default action, either @ALLOW@ or @BLOCK@. If a web
-- request doesn\'t match any of the @Rules@ in a @WebACL@, AWS WAF
-- responds to the request with the default action.
--
-- To create and configure a @WebACL@, perform the following steps:
--
-- 1.  Create and update the @ByteMatchSet@ objects and other predicates
--     that you want to include in @Rules@. For more information, see
--     CreateByteMatchSet, UpdateByteMatchSet, CreateIPSet, UpdateIPSet,
--     CreateSqlInjectionMatchSet, and UpdateSqlInjectionMatchSet.
--
-- 2.  Create and update the @Rules@ that you want to include in the
--     @WebACL@. For more information, see CreateRule and UpdateRule.
--
-- 3.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateWebACL@ request.
--
-- 4.  Submit a @CreateWebACL@ request.
--
-- 5.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateWebACL request.
--
-- 6.  Submit an UpdateWebACL request to specify the @Rules@ that you want
--     to include in the @WebACL@, to specify the default action, and to
--     associate the @WebACL@ with a CloudFront distribution.
--
-- For more information about how to use the AWS WAF API, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.CreateWebACL
  ( -- * Creating a Request
    CreateWebACL (..),
    newCreateWebACL,

    -- * Request Lenses
    createWebACL_tags,
    createWebACL_name,
    createWebACL_metricName,
    createWebACL_defaultAction,
    createWebACL_changeToken,

    -- * Destructuring the Response
    CreateWebACLResponse (..),
    newCreateWebACLResponse,

    -- * Response Lenses
    createWebACLResponse_changeToken,
    createWebACLResponse_webACL,
    createWebACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newCreateWebACL' smart constructor.
data CreateWebACL = CreateWebACL'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A friendly name or description of the WebACL. You can\'t change @Name@
    -- after you create the @WebACL@.
    name :: Prelude.Text,
    -- | A friendly name or description for the metrics for this @WebACL@.The
    -- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
    -- maximum length 128 and minimum length one. It can\'t contain whitespace
    -- or metric names reserved for AWS WAF, including \"All\" and
    -- \"Default_Action.\" You can\'t change @MetricName@ after you create the
    -- @WebACL@.
    metricName :: Prelude.Text,
    -- | The action that you want AWS WAF to take when a request doesn\'t match
    -- the criteria specified in any of the @Rule@ objects that are associated
    -- with the @WebACL@.
    defaultAction :: WafAction,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWebACL_tags' -
--
-- 'name', 'createWebACL_name' - A friendly name or description of the WebACL. You can\'t change @Name@
-- after you create the @WebACL@.
--
-- 'metricName', 'createWebACL_metricName' - A friendly name or description for the metrics for this @WebACL@.The
-- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change @MetricName@ after you create the
-- @WebACL@.
--
-- 'defaultAction', 'createWebACL_defaultAction' - The action that you want AWS WAF to take when a request doesn\'t match
-- the criteria specified in any of the @Rule@ objects that are associated
-- with the @WebACL@.
--
-- 'changeToken', 'createWebACL_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateWebACL ::
  -- | 'name'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'defaultAction'
  WafAction ->
  -- | 'changeToken'
  Prelude.Text ->
  CreateWebACL
newCreateWebACL
  pName_
  pMetricName_
  pDefaultAction_
  pChangeToken_ =
    CreateWebACL'
      { tags = Prelude.Nothing,
        name = pName_,
        metricName = pMetricName_,
        defaultAction = pDefaultAction_,
        changeToken = pChangeToken_
      }

createWebACL_tags :: Lens.Lens' CreateWebACL (Prelude.Maybe (Prelude.NonEmpty Tag))
createWebACL_tags = Lens.lens (\CreateWebACL' {tags} -> tags) (\s@CreateWebACL' {} a -> s {tags = a} :: CreateWebACL) Prelude.. Lens.mapping Lens.coerced

-- | A friendly name or description of the WebACL. You can\'t change @Name@
-- after you create the @WebACL@.
createWebACL_name :: Lens.Lens' CreateWebACL Prelude.Text
createWebACL_name = Lens.lens (\CreateWebACL' {name} -> name) (\s@CreateWebACL' {} a -> s {name = a} :: CreateWebACL)

-- | A friendly name or description for the metrics for this @WebACL@.The
-- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change @MetricName@ after you create the
-- @WebACL@.
createWebACL_metricName :: Lens.Lens' CreateWebACL Prelude.Text
createWebACL_metricName = Lens.lens (\CreateWebACL' {metricName} -> metricName) (\s@CreateWebACL' {} a -> s {metricName = a} :: CreateWebACL)

-- | The action that you want AWS WAF to take when a request doesn\'t match
-- the criteria specified in any of the @Rule@ objects that are associated
-- with the @WebACL@.
createWebACL_defaultAction :: Lens.Lens' CreateWebACL WafAction
createWebACL_defaultAction = Lens.lens (\CreateWebACL' {defaultAction} -> defaultAction) (\s@CreateWebACL' {} a -> s {defaultAction = a} :: CreateWebACL)

-- | The value returned by the most recent call to GetChangeToken.
createWebACL_changeToken :: Lens.Lens' CreateWebACL Prelude.Text
createWebACL_changeToken = Lens.lens (\CreateWebACL' {changeToken} -> changeToken) (\s@CreateWebACL' {} a -> s {changeToken = a} :: CreateWebACL)

instance Core.AWSRequest CreateWebACL where
  type AWSResponse CreateWebACL = CreateWebACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWebACLResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (x Data..?> "WebACL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWebACL where
  hashWithSalt _salt CreateWebACL' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` defaultAction
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData CreateWebACL where
  rnf CreateWebACL' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf changeToken

instance Data.ToHeaders CreateWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.CreateWebACL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWebACL where
  toJSON CreateWebACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("MetricName" Data..= metricName),
            Prelude.Just ("DefaultAction" Data..= defaultAction),
            Prelude.Just ("ChangeToken" Data..= changeToken)
          ]
      )

instance Data.ToPath CreateWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWebACLResponse' smart constructor.
data CreateWebACLResponse = CreateWebACLResponse'
  { -- | The @ChangeToken@ that you used to submit the @CreateWebACL@ request.
    -- You can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The WebACL returned in the @CreateWebACL@ response.
    webACL :: Prelude.Maybe WebACL,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'createWebACLResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateWebACL@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'webACL', 'createWebACLResponse_webACL' - The WebACL returned in the @CreateWebACL@ response.
--
-- 'httpStatus', 'createWebACLResponse_httpStatus' - The response's http status code.
newCreateWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWebACLResponse
newCreateWebACLResponse pHttpStatus_ =
  CreateWebACLResponse'
    { changeToken =
        Prelude.Nothing,
      webACL = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @CreateWebACL@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
createWebACLResponse_changeToken :: Lens.Lens' CreateWebACLResponse (Prelude.Maybe Prelude.Text)
createWebACLResponse_changeToken = Lens.lens (\CreateWebACLResponse' {changeToken} -> changeToken) (\s@CreateWebACLResponse' {} a -> s {changeToken = a} :: CreateWebACLResponse)

-- | The WebACL returned in the @CreateWebACL@ response.
createWebACLResponse_webACL :: Lens.Lens' CreateWebACLResponse (Prelude.Maybe WebACL)
createWebACLResponse_webACL = Lens.lens (\CreateWebACLResponse' {webACL} -> webACL) (\s@CreateWebACLResponse' {} a -> s {webACL = a} :: CreateWebACLResponse)

-- | The response's http status code.
createWebACLResponse_httpStatus :: Lens.Lens' CreateWebACLResponse Prelude.Int
createWebACLResponse_httpStatus = Lens.lens (\CreateWebACLResponse' {httpStatus} -> httpStatus) (\s@CreateWebACLResponse' {} a -> s {httpStatus = a} :: CreateWebACLResponse)

instance Prelude.NFData CreateWebACLResponse where
  rnf CreateWebACLResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf webACL
      `Prelude.seq` Prelude.rnf httpStatus
