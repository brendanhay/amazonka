{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignHook
  ( CampaignHook (..),

    -- * Smart constructor
    mkCampaignHook,

    -- * Lenses
    chLambdaFunctionName,
    chMode,
    chWebURL,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Mode
import qualified Network.AWS.Prelude as Lude

-- | Specifies settings for invoking an AWS Lambda function that customizes a segment for a campaign.
--
-- /See:/ 'mkCampaignHook' smart constructor.
data CampaignHook = CampaignHook'
  { -- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to customize a segment for a campaign.
    lambdaFunctionName :: Lude.Maybe Lude.Text,
    -- | The mode that Amazon Pinpoint uses to invoke the AWS Lambda function. Possible values are:
    --
    --
    --     * FILTER - Invoke the function to customize the segment that's used by a campaign.
    --
    --
    --     * DELIVERY - (Deprecated) Previously, invoked the function to send a campaign through a custom channel. This functionality is not supported anymore. To send a campaign through a custom channel, use the CustomDeliveryConfiguration and CampaignCustomMessage objects of the campaign.
    mode :: Lude.Maybe Mode,
    -- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function over HTTPS.
    webURL :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CampaignHook' with the minimum fields required to make a request.
--
-- * 'lambdaFunctionName' - The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to customize a segment for a campaign.
-- * 'mode' - The mode that Amazon Pinpoint uses to invoke the AWS Lambda function. Possible values are:
--
--
--     * FILTER - Invoke the function to customize the segment that's used by a campaign.
--
--
--     * DELIVERY - (Deprecated) Previously, invoked the function to send a campaign through a custom channel. This functionality is not supported anymore. To send a campaign through a custom channel, use the CustomDeliveryConfiguration and CampaignCustomMessage objects of the campaign.
--
--
-- * 'webURL' - The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function over HTTPS.
mkCampaignHook ::
  CampaignHook
mkCampaignHook =
  CampaignHook'
    { lambdaFunctionName = Lude.Nothing,
      mode = Lude.Nothing,
      webURL = Lude.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to customize a segment for a campaign.
--
-- /Note:/ Consider using 'lambdaFunctionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chLambdaFunctionName :: Lens.Lens' CampaignHook (Lude.Maybe Lude.Text)
chLambdaFunctionName = Lens.lens (lambdaFunctionName :: CampaignHook -> Lude.Maybe Lude.Text) (\s a -> s {lambdaFunctionName = a} :: CampaignHook)
{-# DEPRECATED chLambdaFunctionName "Use generic-lens or generic-optics with 'lambdaFunctionName' instead." #-}

-- | The mode that Amazon Pinpoint uses to invoke the AWS Lambda function. Possible values are:
--
--
--     * FILTER - Invoke the function to customize the segment that's used by a campaign.
--
--
--     * DELIVERY - (Deprecated) Previously, invoked the function to send a campaign through a custom channel. This functionality is not supported anymore. To send a campaign through a custom channel, use the CustomDeliveryConfiguration and CampaignCustomMessage objects of the campaign.
--
--
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chMode :: Lens.Lens' CampaignHook (Lude.Maybe Mode)
chMode = Lens.lens (mode :: CampaignHook -> Lude.Maybe Mode) (\s a -> s {mode = a} :: CampaignHook)
{-# DEPRECATED chMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function over HTTPS.
--
-- /Note:/ Consider using 'webURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chWebURL :: Lens.Lens' CampaignHook (Lude.Maybe Lude.Text)
chWebURL = Lens.lens (webURL :: CampaignHook -> Lude.Maybe Lude.Text) (\s a -> s {webURL = a} :: CampaignHook)
{-# DEPRECATED chWebURL "Use generic-lens or generic-optics with 'webURL' instead." #-}

instance Lude.FromJSON CampaignHook where
  parseJSON =
    Lude.withObject
      "CampaignHook"
      ( \x ->
          CampaignHook'
            Lude.<$> (x Lude..:? "LambdaFunctionName")
            Lude.<*> (x Lude..:? "Mode")
            Lude.<*> (x Lude..:? "WebUrl")
      )

instance Lude.ToJSON CampaignHook where
  toJSON CampaignHook' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LambdaFunctionName" Lude..=) Lude.<$> lambdaFunctionName,
            ("Mode" Lude..=) Lude.<$> mode,
            ("WebUrl" Lude..=) Lude.<$> webURL
          ]
      )
