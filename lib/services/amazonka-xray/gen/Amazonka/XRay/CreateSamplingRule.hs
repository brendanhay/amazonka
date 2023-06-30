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
-- Module      : Amazonka.XRay.CreateSamplingRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule to control sampling behavior for instrumented
-- applications. Services retrieve rules with
-- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingRules.html GetSamplingRules>,
-- and evaluate each rule in ascending order of /priority/ for each
-- request. If a rule matches, the service records a trace, borrowing it
-- from the reservoir size. After 10 seconds, the service reports back to
-- X-Ray with
-- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingTargets.html GetSamplingTargets>
-- to get updated versions of each in-use rule. The updated rule contains a
-- trace quota that the service can use instead of borrowing from the
-- reservoir.
module Amazonka.XRay.CreateSamplingRule
  ( -- * Creating a Request
    CreateSamplingRule (..),
    newCreateSamplingRule,

    -- * Request Lenses
    createSamplingRule_tags,
    createSamplingRule_samplingRule,

    -- * Destructuring the Response
    CreateSamplingRuleResponse (..),
    newCreateSamplingRuleResponse,

    -- * Response Lenses
    createSamplingRuleResponse_samplingRuleRecord,
    createSamplingRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newCreateSamplingRule' smart constructor.
data CreateSamplingRule = CreateSamplingRule'
  { -- | A map that contains one or more tag keys and tag values to attach to an
    -- X-Ray sampling rule. For more information about ways to use tags, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference/.
    --
    -- The following restrictions apply to tags:
    --
    -- -   Maximum number of user-applied tags per resource: 50
    --
    -- -   Maximum tag key length: 128 Unicode characters
    --
    -- -   Maximum tag value length: 256 Unicode characters
    --
    -- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
    --     following characters: _ . : \/ = + - and \@
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for Amazon
    --     Web Services use.
    tags :: Prelude.Maybe [Tag],
    -- | The rule definition.
    samplingRule :: SamplingRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSamplingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSamplingRule_tags' - A map that contains one or more tag keys and tag values to attach to an
-- X-Ray sampling rule. For more information about ways to use tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference/.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of user-applied tags per resource: 50
--
-- -   Maximum tag key length: 128 Unicode characters
--
-- -   Maximum tag value length: 256 Unicode characters
--
-- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
--     following characters: _ . : \/ = + - and \@
--
-- -   Tag keys and values are case sensitive.
--
-- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for Amazon
--     Web Services use.
--
-- 'samplingRule', 'createSamplingRule_samplingRule' - The rule definition.
newCreateSamplingRule ::
  -- | 'samplingRule'
  SamplingRule ->
  CreateSamplingRule
newCreateSamplingRule pSamplingRule_ =
  CreateSamplingRule'
    { tags = Prelude.Nothing,
      samplingRule = pSamplingRule_
    }

-- | A map that contains one or more tag keys and tag values to attach to an
-- X-Ray sampling rule. For more information about ways to use tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference/.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of user-applied tags per resource: 50
--
-- -   Maximum tag key length: 128 Unicode characters
--
-- -   Maximum tag value length: 256 Unicode characters
--
-- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
--     following characters: _ . : \/ = + - and \@
--
-- -   Tag keys and values are case sensitive.
--
-- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for Amazon
--     Web Services use.
createSamplingRule_tags :: Lens.Lens' CreateSamplingRule (Prelude.Maybe [Tag])
createSamplingRule_tags = Lens.lens (\CreateSamplingRule' {tags} -> tags) (\s@CreateSamplingRule' {} a -> s {tags = a} :: CreateSamplingRule) Prelude.. Lens.mapping Lens.coerced

-- | The rule definition.
createSamplingRule_samplingRule :: Lens.Lens' CreateSamplingRule SamplingRule
createSamplingRule_samplingRule = Lens.lens (\CreateSamplingRule' {samplingRule} -> samplingRule) (\s@CreateSamplingRule' {} a -> s {samplingRule = a} :: CreateSamplingRule)

instance Core.AWSRequest CreateSamplingRule where
  type
    AWSResponse CreateSamplingRule =
      CreateSamplingRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSamplingRuleResponse'
            Prelude.<$> (x Data..?> "SamplingRuleRecord")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSamplingRule where
  hashWithSalt _salt CreateSamplingRule' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` samplingRule

instance Prelude.NFData CreateSamplingRule where
  rnf CreateSamplingRule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf samplingRule

instance Data.ToHeaders CreateSamplingRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateSamplingRule where
  toJSON CreateSamplingRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("SamplingRule" Data..= samplingRule)
          ]
      )

instance Data.ToPath CreateSamplingRule where
  toPath = Prelude.const "/CreateSamplingRule"

instance Data.ToQuery CreateSamplingRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSamplingRuleResponse' smart constructor.
data CreateSamplingRuleResponse = CreateSamplingRuleResponse'
  { -- | The saved rule definition and metadata.
    samplingRuleRecord :: Prelude.Maybe SamplingRuleRecord,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSamplingRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingRuleRecord', 'createSamplingRuleResponse_samplingRuleRecord' - The saved rule definition and metadata.
--
-- 'httpStatus', 'createSamplingRuleResponse_httpStatus' - The response's http status code.
newCreateSamplingRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSamplingRuleResponse
newCreateSamplingRuleResponse pHttpStatus_ =
  CreateSamplingRuleResponse'
    { samplingRuleRecord =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The saved rule definition and metadata.
createSamplingRuleResponse_samplingRuleRecord :: Lens.Lens' CreateSamplingRuleResponse (Prelude.Maybe SamplingRuleRecord)
createSamplingRuleResponse_samplingRuleRecord = Lens.lens (\CreateSamplingRuleResponse' {samplingRuleRecord} -> samplingRuleRecord) (\s@CreateSamplingRuleResponse' {} a -> s {samplingRuleRecord = a} :: CreateSamplingRuleResponse)

-- | The response's http status code.
createSamplingRuleResponse_httpStatus :: Lens.Lens' CreateSamplingRuleResponse Prelude.Int
createSamplingRuleResponse_httpStatus = Lens.lens (\CreateSamplingRuleResponse' {httpStatus} -> httpStatus) (\s@CreateSamplingRuleResponse' {} a -> s {httpStatus = a} :: CreateSamplingRuleResponse)

instance Prelude.NFData CreateSamplingRuleResponse where
  rnf CreateSamplingRuleResponse' {..} =
    Prelude.rnf samplingRuleRecord
      `Prelude.seq` Prelude.rnf httpStatus
