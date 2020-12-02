{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookFilterRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The event criteria that specify when a webhook notification is sent to your URL.
--
--
--
-- /See:/ 'webhookFilterRule' smart constructor.
data WebhookFilterRule = WebhookFilterRule'
  { _wfrMatchEquals ::
      !(Maybe Text),
    _wfrJsonPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WebhookFilterRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wfrMatchEquals' - The value selected by the @JsonPath@ expression must match what is supplied in the @MatchEquals@ field. Otherwise, the request is ignored. Properties from the target action configuration can be included as placeholders in this value by surrounding the action configuration key with curly brackets. For example, if the value supplied here is "refs/heads/{Branch}" and the target action has an action configuration property called "Branch" with a value of "master", the @MatchEquals@ value is evaluated as "refs/heads/master". For a list of action configuration properties for built-in action types, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Pipeline Structure Reference Action Requirements> .
--
-- * 'wfrJsonPath' - A JsonPath expression that is applied to the body/payload of the webhook. The value selected by the JsonPath expression must match the value specified in the @MatchEquals@ field. Otherwise, the request is ignored. For more information, see <https://github.com/json-path/JsonPath Java JsonPath implementation> in GitHub.
webhookFilterRule ::
  -- | 'wfrJsonPath'
  Text ->
  WebhookFilterRule
webhookFilterRule pJsonPath_ =
  WebhookFilterRule'
    { _wfrMatchEquals = Nothing,
      _wfrJsonPath = pJsonPath_
    }

-- | The value selected by the @JsonPath@ expression must match what is supplied in the @MatchEquals@ field. Otherwise, the request is ignored. Properties from the target action configuration can be included as placeholders in this value by surrounding the action configuration key with curly brackets. For example, if the value supplied here is "refs/heads/{Branch}" and the target action has an action configuration property called "Branch" with a value of "master", the @MatchEquals@ value is evaluated as "refs/heads/master". For a list of action configuration properties for built-in action types, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Pipeline Structure Reference Action Requirements> .
wfrMatchEquals :: Lens' WebhookFilterRule (Maybe Text)
wfrMatchEquals = lens _wfrMatchEquals (\s a -> s {_wfrMatchEquals = a})

-- | A JsonPath expression that is applied to the body/payload of the webhook. The value selected by the JsonPath expression must match the value specified in the @MatchEquals@ field. Otherwise, the request is ignored. For more information, see <https://github.com/json-path/JsonPath Java JsonPath implementation> in GitHub.
wfrJsonPath :: Lens' WebhookFilterRule Text
wfrJsonPath = lens _wfrJsonPath (\s a -> s {_wfrJsonPath = a})

instance FromJSON WebhookFilterRule where
  parseJSON =
    withObject
      "WebhookFilterRule"
      ( \x ->
          WebhookFilterRule' <$> (x .:? "matchEquals") <*> (x .: "jsonPath")
      )

instance Hashable WebhookFilterRule

instance NFData WebhookFilterRule

instance ToJSON WebhookFilterRule where
  toJSON WebhookFilterRule' {..} =
    object
      ( catMaybes
          [ ("matchEquals" .=) <$> _wfrMatchEquals,
            Just ("jsonPath" .= _wfrJsonPath)
          ]
      )
