{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptRuleSetMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.ReceiptRuleSetMetadata
  ( ReceiptRuleSetMetadata (..)
  -- * Smart constructor
  , mkReceiptRuleSetMetadata
  -- * Lenses
  , rrsmCreatedTimestamp
  , rrsmName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.ReceiptRuleSetName as Types

-- | Information about a receipt rule set.
--
-- A receipt rule set is a collection of rules that specify what Amazon SES should do with mail it receives on behalf of your account's verified domains.
-- For information about setting up receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptRuleSetMetadata' smart constructor.
data ReceiptRuleSetMetadata = ReceiptRuleSetMetadata'
  { createdTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The date and time the receipt rule set was created.
  , name :: Core.Maybe Types.ReceiptRuleSetName
    -- ^ The name of the receipt rule set. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReceiptRuleSetMetadata' value with any optional fields omitted.
mkReceiptRuleSetMetadata
    :: ReceiptRuleSetMetadata
mkReceiptRuleSetMetadata
  = ReceiptRuleSetMetadata'{createdTimestamp = Core.Nothing,
                            name = Core.Nothing}

-- | The date and time the receipt rule set was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsmCreatedTimestamp :: Lens.Lens' ReceiptRuleSetMetadata (Core.Maybe Core.UTCTime)
rrsmCreatedTimestamp = Lens.field @"createdTimestamp"
{-# INLINEABLE rrsmCreatedTimestamp #-}
{-# DEPRECATED createdTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead"  #-}

-- | The name of the receipt rule set. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsmName :: Lens.Lens' ReceiptRuleSetMetadata (Core.Maybe Types.ReceiptRuleSetName)
rrsmName = Lens.field @"name"
{-# INLINEABLE rrsmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML ReceiptRuleSetMetadata where
        parseXML x
          = ReceiptRuleSetMetadata' Core.<$>
              (x Core..@? "CreatedTimestamp") Core.<*> x Core..@? "Name"
