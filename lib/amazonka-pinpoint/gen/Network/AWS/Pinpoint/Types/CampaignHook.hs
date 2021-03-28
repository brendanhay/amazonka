{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.CampaignHook
  ( CampaignHook (..)
  -- * Smart constructor
  , mkCampaignHook
  -- * Lenses
  , chLambdaFunctionName
  , chMode
  , chWebUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Mode as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies settings for invoking an AWS Lambda function that customizes a segment for a campaign.
--
-- /See:/ 'mkCampaignHook' smart constructor.
data CampaignHook = CampaignHook'
  { lambdaFunctionName :: Core.Maybe Core.Text
    -- ^ The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to customize a segment for a campaign.
  , mode :: Core.Maybe Types.Mode
    -- ^ The mode that Amazon Pinpoint uses to invoke the AWS Lambda function. Possible values are:
--
--
--     * FILTER - Invoke the function to customize the segment that's used by a campaign.
--
--
--     * DELIVERY - (Deprecated) Previously, invoked the function to send a campaign through a custom channel. This functionality is not supported anymore. To send a campaign through a custom channel, use the CustomDeliveryConfiguration and CampaignCustomMessage objects of the campaign.
--
--
  , webUrl :: Core.Maybe Core.Text
    -- ^ The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function over HTTPS.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CampaignHook' value with any optional fields omitted.
mkCampaignHook
    :: CampaignHook
mkCampaignHook
  = CampaignHook'{lambdaFunctionName = Core.Nothing,
                  mode = Core.Nothing, webUrl = Core.Nothing}

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to customize a segment for a campaign.
--
-- /Note:/ Consider using 'lambdaFunctionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chLambdaFunctionName :: Lens.Lens' CampaignHook (Core.Maybe Core.Text)
chLambdaFunctionName = Lens.field @"lambdaFunctionName"
{-# INLINEABLE chLambdaFunctionName #-}
{-# DEPRECATED lambdaFunctionName "Use generic-lens or generic-optics with 'lambdaFunctionName' instead"  #-}

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
chMode :: Lens.Lens' CampaignHook (Core.Maybe Types.Mode)
chMode = Lens.field @"mode"
{-# INLINEABLE chMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | The web URL that Amazon Pinpoint calls to invoke the AWS Lambda function over HTTPS.
--
-- /Note:/ Consider using 'webUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chWebUrl :: Lens.Lens' CampaignHook (Core.Maybe Core.Text)
chWebUrl = Lens.field @"webUrl"
{-# INLINEABLE chWebUrl #-}
{-# DEPRECATED webUrl "Use generic-lens or generic-optics with 'webUrl' instead"  #-}

instance Core.FromJSON CampaignHook where
        toJSON CampaignHook{..}
          = Core.object
              (Core.catMaybes
                 [("LambdaFunctionName" Core..=) Core.<$> lambdaFunctionName,
                  ("Mode" Core..=) Core.<$> mode,
                  ("WebUrl" Core..=) Core.<$> webUrl])

instance Core.FromJSON CampaignHook where
        parseJSON
          = Core.withObject "CampaignHook" Core.$
              \ x ->
                CampaignHook' Core.<$>
                  (x Core..:? "LambdaFunctionName") Core.<*> x Core..:? "Mode"
                    Core.<*> x Core..:? "WebUrl"
