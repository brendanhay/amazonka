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
-- Module      : Amazonka.WAFV2.CreateRegexPatternSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a RegexPatternSet, which you reference in a
-- RegexPatternSetReferenceStatement, to have WAF inspect a web request
-- component for the specified patterns.
module Amazonka.WAFV2.CreateRegexPatternSet
  ( -- * Creating a Request
    CreateRegexPatternSet (..),
    newCreateRegexPatternSet,

    -- * Request Lenses
    createRegexPatternSet_description,
    createRegexPatternSet_tags,
    createRegexPatternSet_name,
    createRegexPatternSet_scope,
    createRegexPatternSet_regularExpressionList,

    -- * Destructuring the Response
    CreateRegexPatternSetResponse (..),
    newCreateRegexPatternSetResponse,

    -- * Response Lenses
    createRegexPatternSetResponse_summary,
    createRegexPatternSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newCreateRegexPatternSet' smart constructor.
data CreateRegexPatternSet = CreateRegexPatternSet'
  { -- | A description of the set that helps with identification.
    description :: Prelude.Maybe Prelude.Text,
    -- | An array of key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the set. You cannot change the name after you create the
    -- set.
    name :: Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- or an Amazon Cognito user pool.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope,
    -- | Array of regular expression strings.
    regularExpressionList :: [Regex]
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
-- 'description', 'createRegexPatternSet_description' - A description of the set that helps with identification.
--
-- 'tags', 'createRegexPatternSet_tags' - An array of key:value pairs to associate with the resource.
--
-- 'name', 'createRegexPatternSet_name' - The name of the set. You cannot change the name after you create the
-- set.
--
-- 'scope', 'createRegexPatternSet_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
--
-- 'regularExpressionList', 'createRegexPatternSet_regularExpressionList' - Array of regular expression strings.
newCreateRegexPatternSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  CreateRegexPatternSet
newCreateRegexPatternSet pName_ pScope_ =
  CreateRegexPatternSet'
    { description =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      scope = pScope_,
      regularExpressionList = Prelude.mempty
    }

-- | A description of the set that helps with identification.
createRegexPatternSet_description :: Lens.Lens' CreateRegexPatternSet (Prelude.Maybe Prelude.Text)
createRegexPatternSet_description = Lens.lens (\CreateRegexPatternSet' {description} -> description) (\s@CreateRegexPatternSet' {} a -> s {description = a} :: CreateRegexPatternSet)

-- | An array of key:value pairs to associate with the resource.
createRegexPatternSet_tags :: Lens.Lens' CreateRegexPatternSet (Prelude.Maybe (Prelude.NonEmpty Tag))
createRegexPatternSet_tags = Lens.lens (\CreateRegexPatternSet' {tags} -> tags) (\s@CreateRegexPatternSet' {} a -> s {tags = a} :: CreateRegexPatternSet) Prelude.. Lens.mapping Lens.coerced

-- | The name of the set. You cannot change the name after you create the
-- set.
createRegexPatternSet_name :: Lens.Lens' CreateRegexPatternSet Prelude.Text
createRegexPatternSet_name = Lens.lens (\CreateRegexPatternSet' {name} -> name) (\s@CreateRegexPatternSet' {} a -> s {name = a} :: CreateRegexPatternSet)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
createRegexPatternSet_scope :: Lens.Lens' CreateRegexPatternSet Scope
createRegexPatternSet_scope = Lens.lens (\CreateRegexPatternSet' {scope} -> scope) (\s@CreateRegexPatternSet' {} a -> s {scope = a} :: CreateRegexPatternSet)

-- | Array of regular expression strings.
createRegexPatternSet_regularExpressionList :: Lens.Lens' CreateRegexPatternSet [Regex]
createRegexPatternSet_regularExpressionList = Lens.lens (\CreateRegexPatternSet' {regularExpressionList} -> regularExpressionList) (\s@CreateRegexPatternSet' {} a -> s {regularExpressionList = a} :: CreateRegexPatternSet) Prelude.. Lens.coerced

instance Core.AWSRequest CreateRegexPatternSet where
  type
    AWSResponse CreateRegexPatternSet =
      CreateRegexPatternSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRegexPatternSetResponse'
            Prelude.<$> (x Data..?> "Summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRegexPatternSet where
  hashWithSalt _salt CreateRegexPatternSet' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` regularExpressionList

instance Prelude.NFData CreateRegexPatternSet where
  rnf CreateRegexPatternSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf regularExpressionList

instance Data.ToHeaders CreateRegexPatternSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.CreateRegexPatternSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRegexPatternSet where
  toJSON CreateRegexPatternSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just
              ( "RegularExpressionList"
                  Data..= regularExpressionList
              )
          ]
      )

instance Data.ToPath CreateRegexPatternSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRegexPatternSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRegexPatternSetResponse' smart constructor.
data CreateRegexPatternSetResponse = CreateRegexPatternSetResponse'
  { -- | High-level information about a RegexPatternSet, returned by operations
    -- like create and list. This provides information like the ID, that you
    -- can use to retrieve and manage a @RegexPatternSet@, and the ARN, that
    -- you provide to the RegexPatternSetReferenceStatement to use the pattern
    -- set in a Rule.
    summary :: Prelude.Maybe RegexPatternSetSummary,
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
-- 'summary', 'createRegexPatternSetResponse_summary' - High-level information about a RegexPatternSet, returned by operations
-- like create and list. This provides information like the ID, that you
-- can use to retrieve and manage a @RegexPatternSet@, and the ARN, that
-- you provide to the RegexPatternSetReferenceStatement to use the pattern
-- set in a Rule.
--
-- 'httpStatus', 'createRegexPatternSetResponse_httpStatus' - The response's http status code.
newCreateRegexPatternSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRegexPatternSetResponse
newCreateRegexPatternSetResponse pHttpStatus_ =
  CreateRegexPatternSetResponse'
    { summary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | High-level information about a RegexPatternSet, returned by operations
-- like create and list. This provides information like the ID, that you
-- can use to retrieve and manage a @RegexPatternSet@, and the ARN, that
-- you provide to the RegexPatternSetReferenceStatement to use the pattern
-- set in a Rule.
createRegexPatternSetResponse_summary :: Lens.Lens' CreateRegexPatternSetResponse (Prelude.Maybe RegexPatternSetSummary)
createRegexPatternSetResponse_summary = Lens.lens (\CreateRegexPatternSetResponse' {summary} -> summary) (\s@CreateRegexPatternSetResponse' {} a -> s {summary = a} :: CreateRegexPatternSetResponse)

-- | The response's http status code.
createRegexPatternSetResponse_httpStatus :: Lens.Lens' CreateRegexPatternSetResponse Prelude.Int
createRegexPatternSetResponse_httpStatus = Lens.lens (\CreateRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@CreateRegexPatternSetResponse' {} a -> s {httpStatus = a} :: CreateRegexPatternSetResponse)

instance Prelude.NFData CreateRegexPatternSetResponse where
  rnf CreateRegexPatternSetResponse' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf httpStatus
