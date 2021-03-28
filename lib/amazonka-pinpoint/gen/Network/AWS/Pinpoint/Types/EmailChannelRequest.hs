{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EmailChannelRequest
  ( EmailChannelRequest (..)
  -- * Smart constructor
  , mkEmailChannelRequest
  -- * Lenses
  , ecrfFromAddress
  , ecrfIdentity
  , ecrfConfigurationSet
  , ecrfEnabled
  , ecrfRoleArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status and settings of the email channel for an application.
--
-- /See:/ 'mkEmailChannelRequest' smart constructor.
data EmailChannelRequest = EmailChannelRequest'
  { fromAddress :: Core.Text
    -- ^ The verified email address that you want to send email from when you send email through the channel.
  , identity :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that you want to use when you send email through the channel.
  , configurationSet :: Core.Maybe Core.Text
    -- ^ The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that you want to apply to messages that you send through the channel.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether to enable the email channel for the application.
  , roleArn :: Core.Maybe Core.Text
    -- ^ The ARN of the AWS Identity and Access Management (IAM) role that you want Amazon Pinpoint to use when it submits email-related event data for the channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmailChannelRequest' value with any optional fields omitted.
mkEmailChannelRequest
    :: Core.Text -- ^ 'fromAddress'
    -> Core.Text -- ^ 'identity'
    -> EmailChannelRequest
mkEmailChannelRequest fromAddress identity
  = EmailChannelRequest'{fromAddress, identity,
                         configurationSet = Core.Nothing, enabled = Core.Nothing,
                         roleArn = Core.Nothing}

-- | The verified email address that you want to send email from when you send email through the channel.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrfFromAddress :: Lens.Lens' EmailChannelRequest Core.Text
ecrfFromAddress = Lens.field @"fromAddress"
{-# INLINEABLE ecrfFromAddress #-}
{-# DEPRECATED fromAddress "Use generic-lens or generic-optics with 'fromAddress' instead"  #-}

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that you want to use when you send email through the channel.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrfIdentity :: Lens.Lens' EmailChannelRequest Core.Text
ecrfIdentity = Lens.field @"identity"
{-# INLINEABLE ecrfIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

-- | The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that you want to apply to messages that you send through the channel.
--
-- /Note:/ Consider using 'configurationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrfConfigurationSet :: Lens.Lens' EmailChannelRequest (Core.Maybe Core.Text)
ecrfConfigurationSet = Lens.field @"configurationSet"
{-# INLINEABLE ecrfConfigurationSet #-}
{-# DEPRECATED configurationSet "Use generic-lens or generic-optics with 'configurationSet' instead"  #-}

-- | Specifies whether to enable the email channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrfEnabled :: Lens.Lens' EmailChannelRequest (Core.Maybe Core.Bool)
ecrfEnabled = Lens.field @"enabled"
{-# INLINEABLE ecrfEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that you want Amazon Pinpoint to use when it submits email-related event data for the channel.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrfRoleArn :: Lens.Lens' EmailChannelRequest (Core.Maybe Core.Text)
ecrfRoleArn = Lens.field @"roleArn"
{-# INLINEABLE ecrfRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON EmailChannelRequest where
        toJSON EmailChannelRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FromAddress" Core..= fromAddress),
                  Core.Just ("Identity" Core..= identity),
                  ("ConfigurationSet" Core..=) Core.<$> configurationSet,
                  ("Enabled" Core..=) Core.<$> enabled,
                  ("RoleArn" Core..=) Core.<$> roleArn])
