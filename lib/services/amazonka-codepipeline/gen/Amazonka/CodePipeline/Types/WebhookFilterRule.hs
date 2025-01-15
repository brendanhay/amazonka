{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodePipeline.Types.WebhookFilterRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.WebhookFilterRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The event criteria that specify when a webhook notification is sent to
-- your URL.
--
-- /See:/ 'newWebhookFilterRule' smart constructor.
data WebhookFilterRule = WebhookFilterRule'
  { -- | The value selected by the @JsonPath@ expression must match what is
    -- supplied in the @MatchEquals@ field. Otherwise, the request is ignored.
    -- Properties from the target action configuration can be included as
    -- placeholders in this value by surrounding the action configuration key
    -- with curly brackets. For example, if the value supplied here is
    -- \"refs\/heads\/{Branch}\" and the target action has an action
    -- configuration property called \"Branch\" with a value of \"master\", the
    -- @MatchEquals@ value is evaluated as \"refs\/heads\/master\". For a list
    -- of action configuration properties for built-in action types, see
    -- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Pipeline Structure Reference Action Requirements>.
    matchEquals :: Prelude.Maybe Prelude.Text,
    -- | A JsonPath expression that is applied to the body\/payload of the
    -- webhook. The value selected by the JsonPath expression must match the
    -- value specified in the @MatchEquals@ field. Otherwise, the request is
    -- ignored. For more information, see
    -- <https://github.com/json-path/JsonPath Java JsonPath implementation> in
    -- GitHub.
    jsonPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebhookFilterRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchEquals', 'webhookFilterRule_matchEquals' - The value selected by the @JsonPath@ expression must match what is
-- supplied in the @MatchEquals@ field. Otherwise, the request is ignored.
-- Properties from the target action configuration can be included as
-- placeholders in this value by surrounding the action configuration key
-- with curly brackets. For example, if the value supplied here is
-- \"refs\/heads\/{Branch}\" and the target action has an action
-- configuration property called \"Branch\" with a value of \"master\", the
-- @MatchEquals@ value is evaluated as \"refs\/heads\/master\". For a list
-- of action configuration properties for built-in action types, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Pipeline Structure Reference Action Requirements>.
--
-- 'jsonPath', 'webhookFilterRule_jsonPath' - A JsonPath expression that is applied to the body\/payload of the
-- webhook. The value selected by the JsonPath expression must match the
-- value specified in the @MatchEquals@ field. Otherwise, the request is
-- ignored. For more information, see
-- <https://github.com/json-path/JsonPath Java JsonPath implementation> in
-- GitHub.
newWebhookFilterRule ::
  -- | 'jsonPath'
  Prelude.Text ->
  WebhookFilterRule
newWebhookFilterRule pJsonPath_ =
  WebhookFilterRule'
    { matchEquals = Prelude.Nothing,
      jsonPath = pJsonPath_
    }

-- | The value selected by the @JsonPath@ expression must match what is
-- supplied in the @MatchEquals@ field. Otherwise, the request is ignored.
-- Properties from the target action configuration can be included as
-- placeholders in this value by surrounding the action configuration key
-- with curly brackets. For example, if the value supplied here is
-- \"refs\/heads\/{Branch}\" and the target action has an action
-- configuration property called \"Branch\" with a value of \"master\", the
-- @MatchEquals@ value is evaluated as \"refs\/heads\/master\". For a list
-- of action configuration properties for built-in action types, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Pipeline Structure Reference Action Requirements>.
webhookFilterRule_matchEquals :: Lens.Lens' WebhookFilterRule (Prelude.Maybe Prelude.Text)
webhookFilterRule_matchEquals = Lens.lens (\WebhookFilterRule' {matchEquals} -> matchEquals) (\s@WebhookFilterRule' {} a -> s {matchEquals = a} :: WebhookFilterRule)

-- | A JsonPath expression that is applied to the body\/payload of the
-- webhook. The value selected by the JsonPath expression must match the
-- value specified in the @MatchEquals@ field. Otherwise, the request is
-- ignored. For more information, see
-- <https://github.com/json-path/JsonPath Java JsonPath implementation> in
-- GitHub.
webhookFilterRule_jsonPath :: Lens.Lens' WebhookFilterRule Prelude.Text
webhookFilterRule_jsonPath = Lens.lens (\WebhookFilterRule' {jsonPath} -> jsonPath) (\s@WebhookFilterRule' {} a -> s {jsonPath = a} :: WebhookFilterRule)

instance Data.FromJSON WebhookFilterRule where
  parseJSON =
    Data.withObject
      "WebhookFilterRule"
      ( \x ->
          WebhookFilterRule'
            Prelude.<$> (x Data..:? "matchEquals")
            Prelude.<*> (x Data..: "jsonPath")
      )

instance Prelude.Hashable WebhookFilterRule where
  hashWithSalt _salt WebhookFilterRule' {..} =
    _salt
      `Prelude.hashWithSalt` matchEquals
      `Prelude.hashWithSalt` jsonPath

instance Prelude.NFData WebhookFilterRule where
  rnf WebhookFilterRule' {..} =
    Prelude.rnf matchEquals `Prelude.seq`
      Prelude.rnf jsonPath

instance Data.ToJSON WebhookFilterRule where
  toJSON WebhookFilterRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("matchEquals" Data..=) Prelude.<$> matchEquals,
            Prelude.Just ("jsonPath" Data..= jsonPath)
          ]
      )
