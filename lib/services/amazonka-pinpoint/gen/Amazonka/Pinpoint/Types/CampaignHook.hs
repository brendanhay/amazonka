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
-- Module      : Amazonka.Pinpoint.Types.CampaignHook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignHook where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.Mode
import qualified Amazonka.Prelude as Prelude

-- | Specifies settings for invoking an AWS Lambda function that customizes a
-- segment for a campaign.
--
-- /See:/ 'newCampaignHook' smart constructor.
data CampaignHook = CampaignHook'
  { -- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that
    -- Amazon Pinpoint invokes to customize a segment for a campaign.
    lambdaFunctionName :: Prelude.Maybe Prelude.Text,
    -- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function
    -- over HTTPS.
    webUrl :: Prelude.Maybe Prelude.Text,
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
    mode :: Prelude.Maybe Mode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctionName', 'campaignHook_lambdaFunctionName' - The name or Amazon Resource Name (ARN) of the AWS Lambda function that
-- Amazon Pinpoint invokes to customize a segment for a campaign.
--
-- 'webUrl', 'campaignHook_webUrl' - The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function
-- over HTTPS.
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
newCampaignHook ::
  CampaignHook
newCampaignHook =
  CampaignHook'
    { lambdaFunctionName = Prelude.Nothing,
      webUrl = Prelude.Nothing,
      mode = Prelude.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that
-- Amazon Pinpoint invokes to customize a segment for a campaign.
campaignHook_lambdaFunctionName :: Lens.Lens' CampaignHook (Prelude.Maybe Prelude.Text)
campaignHook_lambdaFunctionName = Lens.lens (\CampaignHook' {lambdaFunctionName} -> lambdaFunctionName) (\s@CampaignHook' {} a -> s {lambdaFunctionName = a} :: CampaignHook)

-- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function
-- over HTTPS.
campaignHook_webUrl :: Lens.Lens' CampaignHook (Prelude.Maybe Prelude.Text)
campaignHook_webUrl = Lens.lens (\CampaignHook' {webUrl} -> webUrl) (\s@CampaignHook' {} a -> s {webUrl = a} :: CampaignHook)

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
campaignHook_mode :: Lens.Lens' CampaignHook (Prelude.Maybe Mode)
campaignHook_mode = Lens.lens (\CampaignHook' {mode} -> mode) (\s@CampaignHook' {} a -> s {mode = a} :: CampaignHook)

instance Core.FromJSON CampaignHook where
  parseJSON =
    Core.withObject
      "CampaignHook"
      ( \x ->
          CampaignHook'
            Prelude.<$> (x Core..:? "LambdaFunctionName")
            Prelude.<*> (x Core..:? "WebUrl")
            Prelude.<*> (x Core..:? "Mode")
      )

instance Prelude.Hashable CampaignHook where
  hashWithSalt _salt CampaignHook' {..} =
    _salt `Prelude.hashWithSalt` lambdaFunctionName
      `Prelude.hashWithSalt` webUrl
      `Prelude.hashWithSalt` mode

instance Prelude.NFData CampaignHook where
  rnf CampaignHook' {..} =
    Prelude.rnf lambdaFunctionName
      `Prelude.seq` Prelude.rnf webUrl
      `Prelude.seq` Prelude.rnf mode

instance Core.ToJSON CampaignHook where
  toJSON CampaignHook' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LambdaFunctionName" Core..=)
              Prelude.<$> lambdaFunctionName,
            ("WebUrl" Core..=) Prelude.<$> webUrl,
            ("Mode" Core..=) Prelude.<$> mode
          ]
      )
