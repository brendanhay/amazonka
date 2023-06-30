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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignHook where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    mode :: Prelude.Maybe Mode,
    -- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function
    -- over HTTPS.
    webUrl :: Prelude.Maybe Prelude.Text
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
-- 'webUrl', 'campaignHook_webUrl' - The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function
-- over HTTPS.
newCampaignHook ::
  CampaignHook
newCampaignHook =
  CampaignHook'
    { lambdaFunctionName = Prelude.Nothing,
      mode = Prelude.Nothing,
      webUrl = Prelude.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that
-- Amazon Pinpoint invokes to customize a segment for a campaign.
campaignHook_lambdaFunctionName :: Lens.Lens' CampaignHook (Prelude.Maybe Prelude.Text)
campaignHook_lambdaFunctionName = Lens.lens (\CampaignHook' {lambdaFunctionName} -> lambdaFunctionName) (\s@CampaignHook' {} a -> s {lambdaFunctionName = a} :: CampaignHook)

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

-- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function
-- over HTTPS.
campaignHook_webUrl :: Lens.Lens' CampaignHook (Prelude.Maybe Prelude.Text)
campaignHook_webUrl = Lens.lens (\CampaignHook' {webUrl} -> webUrl) (\s@CampaignHook' {} a -> s {webUrl = a} :: CampaignHook)

instance Data.FromJSON CampaignHook where
  parseJSON =
    Data.withObject
      "CampaignHook"
      ( \x ->
          CampaignHook'
            Prelude.<$> (x Data..:? "LambdaFunctionName")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "WebUrl")
      )

instance Prelude.Hashable CampaignHook where
  hashWithSalt _salt CampaignHook' {..} =
    _salt
      `Prelude.hashWithSalt` lambdaFunctionName
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` webUrl

instance Prelude.NFData CampaignHook where
  rnf CampaignHook' {..} =
    Prelude.rnf lambdaFunctionName
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf webUrl

instance Data.ToJSON CampaignHook where
  toJSON CampaignHook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LambdaFunctionName" Data..=)
              Prelude.<$> lambdaFunctionName,
            ("Mode" Data..=) Prelude.<$> mode,
            ("WebUrl" Data..=) Prelude.<$> webUrl
          ]
      )
