{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about the settings for an action type.
--
--
--
-- /See:/ 'actionTypeSettings' smart constructor.
data ActionTypeSettings = ActionTypeSettings'
  { _atsThirdPartyConfigurationURL ::
      !(Maybe Text),
    _atsExecutionURLTemplate :: !(Maybe Text),
    _atsRevisionURLTemplate :: !(Maybe Text),
    _atsEntityURLTemplate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionTypeSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atsThirdPartyConfigurationURL' - The URL of a sign-up page where users can sign up for an external service and perform initial configuration of the action provided by that service.
--
-- * 'atsExecutionURLTemplate' - The URL returned to the AWS CodePipeline console that contains a link to the top-level landing page for the external system, such as the console page for AWS CodeDeploy. This link is shown on the pipeline view page in the AWS CodePipeline console and provides a link to the execution entity of the external action.
--
-- * 'atsRevisionURLTemplate' - The URL returned to the AWS CodePipeline console that contains a link to the page where customers can update or change the configuration of the external action.
--
-- * 'atsEntityURLTemplate' - The URL returned to the AWS CodePipeline console that provides a deep link to the resources of the external system, such as the configuration page for an AWS CodeDeploy deployment group. This link is provided as part of the action display in the pipeline.
actionTypeSettings ::
  ActionTypeSettings
actionTypeSettings =
  ActionTypeSettings'
    { _atsThirdPartyConfigurationURL = Nothing,
      _atsExecutionURLTemplate = Nothing,
      _atsRevisionURLTemplate = Nothing,
      _atsEntityURLTemplate = Nothing
    }

-- | The URL of a sign-up page where users can sign up for an external service and perform initial configuration of the action provided by that service.
atsThirdPartyConfigurationURL :: Lens' ActionTypeSettings (Maybe Text)
atsThirdPartyConfigurationURL = lens _atsThirdPartyConfigurationURL (\s a -> s {_atsThirdPartyConfigurationURL = a})

-- | The URL returned to the AWS CodePipeline console that contains a link to the top-level landing page for the external system, such as the console page for AWS CodeDeploy. This link is shown on the pipeline view page in the AWS CodePipeline console and provides a link to the execution entity of the external action.
atsExecutionURLTemplate :: Lens' ActionTypeSettings (Maybe Text)
atsExecutionURLTemplate = lens _atsExecutionURLTemplate (\s a -> s {_atsExecutionURLTemplate = a})

-- | The URL returned to the AWS CodePipeline console that contains a link to the page where customers can update or change the configuration of the external action.
atsRevisionURLTemplate :: Lens' ActionTypeSettings (Maybe Text)
atsRevisionURLTemplate = lens _atsRevisionURLTemplate (\s a -> s {_atsRevisionURLTemplate = a})

-- | The URL returned to the AWS CodePipeline console that provides a deep link to the resources of the external system, such as the configuration page for an AWS CodeDeploy deployment group. This link is provided as part of the action display in the pipeline.
atsEntityURLTemplate :: Lens' ActionTypeSettings (Maybe Text)
atsEntityURLTemplate = lens _atsEntityURLTemplate (\s a -> s {_atsEntityURLTemplate = a})

instance FromJSON ActionTypeSettings where
  parseJSON =
    withObject
      "ActionTypeSettings"
      ( \x ->
          ActionTypeSettings'
            <$> (x .:? "thirdPartyConfigurationUrl")
            <*> (x .:? "executionUrlTemplate")
            <*> (x .:? "revisionUrlTemplate")
            <*> (x .:? "entityUrlTemplate")
      )

instance Hashable ActionTypeSettings

instance NFData ActionTypeSettings

instance ToJSON ActionTypeSettings where
  toJSON ActionTypeSettings' {..} =
    object
      ( catMaybes
          [ ("thirdPartyConfigurationUrl" .=)
              <$> _atsThirdPartyConfigurationURL,
            ("executionUrlTemplate" .=) <$> _atsExecutionURLTemplate,
            ("revisionUrlTemplate" .=) <$> _atsRevisionURLTemplate,
            ("entityUrlTemplate" .=) <$> _atsEntityURLTemplate
          ]
      )
