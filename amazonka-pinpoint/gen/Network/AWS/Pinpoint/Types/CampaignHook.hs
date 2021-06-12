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
-- Module      : Network.AWS.Pinpoint.Types.CampaignHook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignHook where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Mode

-- | Specifies settings for invoking an AWS Lambda function that customizes a
-- segment for a campaign.
--
-- /See:/ 'newCampaignHook' smart constructor.
data CampaignHook = CampaignHook'
  { -- | The mode that Amazon Pinpoint uses to invoke the AWS Lambda function.
    -- Possible values are:
    --
    -- -   FILTER - Invoke the function to customize the segment that\'s used
    --     by a campaign.
    --
    -- -   DELIVERY - (Deprecated) Previously, invoked the function to send a
    --     campaign through a custom channel. This functionality is not
    --     supported anymore. To send a campaign through a custom channel, use
    --     the CustomDeliveryConfiguration and CampaignCustomMessage objects of
    --     the campaign.
    mode :: Core.Maybe Mode,
    -- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that
    -- Amazon Pinpoint invokes to customize a segment for a campaign.
    lambdaFunctionName :: Core.Maybe Core.Text,
    -- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function
    -- over HTTPS.
    webUrl :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CampaignHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'campaignHook_mode' - The mode that Amazon Pinpoint uses to invoke the AWS Lambda function.
-- Possible values are:
--
-- -   FILTER - Invoke the function to customize the segment that\'s used
--     by a campaign.
--
-- -   DELIVERY - (Deprecated) Previously, invoked the function to send a
--     campaign through a custom channel. This functionality is not
--     supported anymore. To send a campaign through a custom channel, use
--     the CustomDeliveryConfiguration and CampaignCustomMessage objects of
--     the campaign.
--
-- 'lambdaFunctionName', 'campaignHook_lambdaFunctionName' - The name or Amazon Resource Name (ARN) of the AWS Lambda function that
-- Amazon Pinpoint invokes to customize a segment for a campaign.
--
-- 'webUrl', 'campaignHook_webUrl' - The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function
-- over HTTPS.
newCampaignHook ::
  CampaignHook
newCampaignHook =
  CampaignHook'
    { mode = Core.Nothing,
      lambdaFunctionName = Core.Nothing,
      webUrl = Core.Nothing
    }

-- | The mode that Amazon Pinpoint uses to invoke the AWS Lambda function.
-- Possible values are:
--
-- -   FILTER - Invoke the function to customize the segment that\'s used
--     by a campaign.
--
-- -   DELIVERY - (Deprecated) Previously, invoked the function to send a
--     campaign through a custom channel. This functionality is not
--     supported anymore. To send a campaign through a custom channel, use
--     the CustomDeliveryConfiguration and CampaignCustomMessage objects of
--     the campaign.
campaignHook_mode :: Lens.Lens' CampaignHook (Core.Maybe Mode)
campaignHook_mode = Lens.lens (\CampaignHook' {mode} -> mode) (\s@CampaignHook' {} a -> s {mode = a} :: CampaignHook)

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that
-- Amazon Pinpoint invokes to customize a segment for a campaign.
campaignHook_lambdaFunctionName :: Lens.Lens' CampaignHook (Core.Maybe Core.Text)
campaignHook_lambdaFunctionName = Lens.lens (\CampaignHook' {lambdaFunctionName} -> lambdaFunctionName) (\s@CampaignHook' {} a -> s {lambdaFunctionName = a} :: CampaignHook)

-- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function
-- over HTTPS.
campaignHook_webUrl :: Lens.Lens' CampaignHook (Core.Maybe Core.Text)
campaignHook_webUrl = Lens.lens (\CampaignHook' {webUrl} -> webUrl) (\s@CampaignHook' {} a -> s {webUrl = a} :: CampaignHook)

instance Core.FromJSON CampaignHook where
  parseJSON =
    Core.withObject
      "CampaignHook"
      ( \x ->
          CampaignHook'
            Core.<$> (x Core..:? "Mode")
            Core.<*> (x Core..:? "LambdaFunctionName")
            Core.<*> (x Core..:? "WebUrl")
      )

instance Core.Hashable CampaignHook

instance Core.NFData CampaignHook

instance Core.ToJSON CampaignHook where
  toJSON CampaignHook' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Mode" Core..=) Core.<$> mode,
            ("LambdaFunctionName" Core..=)
              Core.<$> lambdaFunctionName,
            ("WebUrl" Core..=) Core.<$> webUrl
          ]
      )
