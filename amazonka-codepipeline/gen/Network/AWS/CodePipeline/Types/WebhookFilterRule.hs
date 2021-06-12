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
-- Module      : Network.AWS.CodePipeline.Types.WebhookFilterRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookFilterRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    matchEquals :: Core.Maybe Core.Text,
    -- | A JsonPath expression that is applied to the body\/payload of the
    -- webhook. The value selected by the JsonPath expression must match the
    -- value specified in the @MatchEquals@ field. Otherwise, the request is
    -- ignored. For more information, see
    -- <https://github.com/json-path/JsonPath Java JsonPath implementation> in
    -- GitHub.
    jsonPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  WebhookFilterRule
newWebhookFilterRule pJsonPath_ =
  WebhookFilterRule'
    { matchEquals = Core.Nothing,
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
webhookFilterRule_matchEquals :: Lens.Lens' WebhookFilterRule (Core.Maybe Core.Text)
webhookFilterRule_matchEquals = Lens.lens (\WebhookFilterRule' {matchEquals} -> matchEquals) (\s@WebhookFilterRule' {} a -> s {matchEquals = a} :: WebhookFilterRule)

-- | A JsonPath expression that is applied to the body\/payload of the
-- webhook. The value selected by the JsonPath expression must match the
-- value specified in the @MatchEquals@ field. Otherwise, the request is
-- ignored. For more information, see
-- <https://github.com/json-path/JsonPath Java JsonPath implementation> in
-- GitHub.
webhookFilterRule_jsonPath :: Lens.Lens' WebhookFilterRule Core.Text
webhookFilterRule_jsonPath = Lens.lens (\WebhookFilterRule' {jsonPath} -> jsonPath) (\s@WebhookFilterRule' {} a -> s {jsonPath = a} :: WebhookFilterRule)

instance Core.FromJSON WebhookFilterRule where
  parseJSON =
    Core.withObject
      "WebhookFilterRule"
      ( \x ->
          WebhookFilterRule'
            Core.<$> (x Core..:? "matchEquals")
            Core.<*> (x Core..: "jsonPath")
      )

instance Core.Hashable WebhookFilterRule

instance Core.NFData WebhookFilterRule

instance Core.ToJSON WebhookFilterRule where
  toJSON WebhookFilterRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("matchEquals" Core..=) Core.<$> matchEquals,
            Core.Just ("jsonPath" Core..= jsonPath)
          ]
      )
