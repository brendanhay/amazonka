{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptRuleSetMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptRuleSetMetadata
  ( ReceiptRuleSetMetadata (..),

    -- * Smart constructor
    mkReceiptRuleSetMetadata,

    -- * Lenses
    rrsmName,
    rrsmCreatedTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a receipt rule set.
--
-- A receipt rule set is a collection of rules that specify what Amazon SES should do with mail it receives on behalf of your account's verified domains.
-- For information about setting up receipt rule sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptRuleSetMetadata' smart constructor.
data ReceiptRuleSetMetadata = ReceiptRuleSetMetadata'
  { name ::
      Lude.Maybe Lude.Text,
    createdTimestamp :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReceiptRuleSetMetadata' with the minimum fields required to make a request.
--
-- * 'createdTimestamp' - The date and time the receipt rule set was created.
-- * 'name' - The name of the receipt rule set. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
mkReceiptRuleSetMetadata ::
  ReceiptRuleSetMetadata
mkReceiptRuleSetMetadata =
  ReceiptRuleSetMetadata'
    { name = Lude.Nothing,
      createdTimestamp = Lude.Nothing
    }

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
rrsmName :: Lens.Lens' ReceiptRuleSetMetadata (Lude.Maybe Lude.Text)
rrsmName = Lens.lens (name :: ReceiptRuleSetMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ReceiptRuleSetMetadata)
{-# DEPRECATED rrsmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time the receipt rule set was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsmCreatedTimestamp :: Lens.Lens' ReceiptRuleSetMetadata (Lude.Maybe Lude.DateTime)
rrsmCreatedTimestamp = Lens.lens (createdTimestamp :: ReceiptRuleSetMetadata -> Lude.Maybe Lude.DateTime) (\s a -> s {createdTimestamp = a} :: ReceiptRuleSetMetadata)
{-# DEPRECATED rrsmCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

instance Lude.FromXML ReceiptRuleSetMetadata where
  parseXML x =
    ReceiptRuleSetMetadata'
      Lude.<$> (x Lude..@? "Name") Lude.<*> (x Lude..@? "CreatedTimestamp")
