{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignHook where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Mode
import Network.AWS.Prelude

-- | Specifies settings for invoking an AWS Lambda function that customizes a segment for a campaign.
--
--
--
-- /See:/ 'campaignHook' smart constructor.
data CampaignHook = CampaignHook'
  { _chLambdaFunctionName ::
      !(Maybe Text),
    _chMode :: !(Maybe Mode),
    _chWebURL :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chLambdaFunctionName' - The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to customize a segment for a campaign.
--
-- * 'chMode' - The mode that Amazon Pinpoint uses to invoke the AWS Lambda function. Possible values are:     * FILTER - Invoke the function to customize the segment that's used by a campaign.     * DELIVERY - (Deprecated) Previously, invoked the function to send a campaign through a custom channel. This functionality is not supported anymore. To send a campaign through a custom channel, use the CustomDeliveryConfiguration and CampaignCustomMessage objects of the campaign.
--
-- * 'chWebURL' - The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function over HTTPS.
campaignHook ::
  CampaignHook
campaignHook =
  CampaignHook'
    { _chLambdaFunctionName = Nothing,
      _chMode = Nothing,
      _chWebURL = Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to customize a segment for a campaign.
chLambdaFunctionName :: Lens' CampaignHook (Maybe Text)
chLambdaFunctionName = lens _chLambdaFunctionName (\s a -> s {_chLambdaFunctionName = a})

-- | The mode that Amazon Pinpoint uses to invoke the AWS Lambda function. Possible values are:     * FILTER - Invoke the function to customize the segment that's used by a campaign.     * DELIVERY - (Deprecated) Previously, invoked the function to send a campaign through a custom channel. This functionality is not supported anymore. To send a campaign through a custom channel, use the CustomDeliveryConfiguration and CampaignCustomMessage objects of the campaign.
chMode :: Lens' CampaignHook (Maybe Mode)
chMode = lens _chMode (\s a -> s {_chMode = a})

-- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function over HTTPS.
chWebURL :: Lens' CampaignHook (Maybe Text)
chWebURL = lens _chWebURL (\s a -> s {_chWebURL = a})

instance FromJSON CampaignHook where
  parseJSON =
    withObject
      "CampaignHook"
      ( \x ->
          CampaignHook'
            <$> (x .:? "LambdaFunctionName")
            <*> (x .:? "Mode")
            <*> (x .:? "WebUrl")
      )

instance Hashable CampaignHook

instance NFData CampaignHook

instance ToJSON CampaignHook where
  toJSON CampaignHook' {..} =
    object
      ( catMaybes
          [ ("LambdaFunctionName" .=) <$> _chLambdaFunctionName,
            ("Mode" .=) <$> _chMode,
            ("WebUrl" .=) <$> _chWebURL
          ]
      )
