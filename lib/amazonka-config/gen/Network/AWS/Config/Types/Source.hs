{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.Source
  ( Source (..)
  -- * Smart constructor
  , mkSource
  -- * Lenses
  , sOwner
  , sSourceIdentifier
  , sSourceDetails
  ) where

import qualified Network.AWS.Config.Types.Owner as Types
import qualified Network.AWS.Config.Types.SourceDetail as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the AWS Config rule owner (AWS or customer), the rule identifier, and the events that trigger the evaluation of your AWS resources.
--
-- /See:/ 'mkSource' smart constructor.
data Source = Source'
  { owner :: Types.Owner
    -- ^ Indicates whether AWS or the customer owns and manages the AWS Config rule.
  , sourceIdentifier :: Types.StringWithCharLimit256
    -- ^ For AWS Config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> .
--
-- For custom rules, the identifier is the Amazon Resource Name (ARN) of the rule's AWS Lambda function, such as @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@ .
  , sourceDetails :: Core.Maybe [Types.SourceDetail]
    -- ^ Provides the source and type of the event that causes AWS Config to evaluate your AWS resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Source' value with any optional fields omitted.
mkSource
    :: Types.Owner -- ^ 'owner'
    -> Types.StringWithCharLimit256 -- ^ 'sourceIdentifier'
    -> Source
mkSource owner sourceIdentifier
  = Source'{owner, sourceIdentifier, sourceDetails = Core.Nothing}

-- | Indicates whether AWS or the customer owns and manages the AWS Config rule.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwner :: Lens.Lens' Source Types.Owner
sOwner = Lens.field @"owner"
{-# INLINEABLE sOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | For AWS Config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> .
--
-- For custom rules, the identifier is the Amazon Resource Name (ARN) of the rule's AWS Lambda function, such as @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@ .
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourceIdentifier :: Lens.Lens' Source Types.StringWithCharLimit256
sSourceIdentifier = Lens.field @"sourceIdentifier"
{-# INLINEABLE sSourceIdentifier #-}
{-# DEPRECATED sourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead"  #-}

-- | Provides the source and type of the event that causes AWS Config to evaluate your AWS resources.
--
-- /Note:/ Consider using 'sourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourceDetails :: Lens.Lens' Source (Core.Maybe [Types.SourceDetail])
sSourceDetails = Lens.field @"sourceDetails"
{-# INLINEABLE sSourceDetails #-}
{-# DEPRECATED sourceDetails "Use generic-lens or generic-optics with 'sourceDetails' instead"  #-}

instance Core.FromJSON Source where
        toJSON Source{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Owner" Core..= owner),
                  Core.Just ("SourceIdentifier" Core..= sourceIdentifier),
                  ("SourceDetails" Core..=) Core.<$> sourceDetails])

instance Core.FromJSON Source where
        parseJSON
          = Core.withObject "Source" Core.$
              \ x ->
                Source' Core.<$>
                  (x Core..: "Owner") Core.<*> x Core..: "SourceIdentifier" Core.<*>
                    x Core..:? "SourceDetails"
