{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookFilterRule
  ( WebhookFilterRule (..),

    -- * Smart constructor
    mkWebhookFilterRule,

    -- * Lenses
    wfrMatchEquals,
    wfrJsonPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The event criteria that specify when a webhook notification is sent to your URL.
--
-- /See:/ 'mkWebhookFilterRule' smart constructor.
data WebhookFilterRule = WebhookFilterRule'
  { -- | The value selected by the @JsonPath@ expression must match what is supplied in the @MatchEquals@ field. Otherwise, the request is ignored. Properties from the target action configuration can be included as placeholders in this value by surrounding the action configuration key with curly brackets. For example, if the value supplied here is "refs/heads/{Branch}" and the target action has an action configuration property called "Branch" with a value of "master", the @MatchEquals@ value is evaluated as "refs/heads/master". For a list of action configuration properties for built-in action types, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Pipeline Structure Reference Action Requirements> .
    matchEquals :: Lude.Maybe Lude.Text,
    -- | A JsonPath expression that is applied to the body/payload of the webhook. The value selected by the JsonPath expression must match the value specified in the @MatchEquals@ field. Otherwise, the request is ignored. For more information, see <https://github.com/json-path/JsonPath Java JsonPath implementation> in GitHub.
    jsonPath :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WebhookFilterRule' with the minimum fields required to make a request.
--
-- * 'matchEquals' - The value selected by the @JsonPath@ expression must match what is supplied in the @MatchEquals@ field. Otherwise, the request is ignored. Properties from the target action configuration can be included as placeholders in this value by surrounding the action configuration key with curly brackets. For example, if the value supplied here is "refs/heads/{Branch}" and the target action has an action configuration property called "Branch" with a value of "master", the @MatchEquals@ value is evaluated as "refs/heads/master". For a list of action configuration properties for built-in action types, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Pipeline Structure Reference Action Requirements> .
-- * 'jsonPath' - A JsonPath expression that is applied to the body/payload of the webhook. The value selected by the JsonPath expression must match the value specified in the @MatchEquals@ field. Otherwise, the request is ignored. For more information, see <https://github.com/json-path/JsonPath Java JsonPath implementation> in GitHub.
mkWebhookFilterRule ::
  -- | 'jsonPath'
  Lude.Text ->
  WebhookFilterRule
mkWebhookFilterRule pJsonPath_ =
  WebhookFilterRule'
    { matchEquals = Lude.Nothing,
      jsonPath = pJsonPath_
    }

-- | The value selected by the @JsonPath@ expression must match what is supplied in the @MatchEquals@ field. Otherwise, the request is ignored. Properties from the target action configuration can be included as placeholders in this value by surrounding the action configuration key with curly brackets. For example, if the value supplied here is "refs/heads/{Branch}" and the target action has an action configuration property called "Branch" with a value of "master", the @MatchEquals@ value is evaluated as "refs/heads/master". For a list of action configuration properties for built-in action types, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Pipeline Structure Reference Action Requirements> .
--
-- /Note:/ Consider using 'matchEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfrMatchEquals :: Lens.Lens' WebhookFilterRule (Lude.Maybe Lude.Text)
wfrMatchEquals = Lens.lens (matchEquals :: WebhookFilterRule -> Lude.Maybe Lude.Text) (\s a -> s {matchEquals = a} :: WebhookFilterRule)
{-# DEPRECATED wfrMatchEquals "Use generic-lens or generic-optics with 'matchEquals' instead." #-}

-- | A JsonPath expression that is applied to the body/payload of the webhook. The value selected by the JsonPath expression must match the value specified in the @MatchEquals@ field. Otherwise, the request is ignored. For more information, see <https://github.com/json-path/JsonPath Java JsonPath implementation> in GitHub.
--
-- /Note:/ Consider using 'jsonPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wfrJsonPath :: Lens.Lens' WebhookFilterRule Lude.Text
wfrJsonPath = Lens.lens (jsonPath :: WebhookFilterRule -> Lude.Text) (\s a -> s {jsonPath = a} :: WebhookFilterRule)
{-# DEPRECATED wfrJsonPath "Use generic-lens or generic-optics with 'jsonPath' instead." #-}

instance Lude.FromJSON WebhookFilterRule where
  parseJSON =
    Lude.withObject
      "WebhookFilterRule"
      ( \x ->
          WebhookFilterRule'
            Lude.<$> (x Lude..:? "matchEquals") Lude.<*> (x Lude..: "jsonPath")
      )

instance Lude.ToJSON WebhookFilterRule where
  toJSON WebhookFilterRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("matchEquals" Lude..=) Lude.<$> matchEquals,
            Lude.Just ("jsonPath" Lude..= jsonPath)
          ]
      )
