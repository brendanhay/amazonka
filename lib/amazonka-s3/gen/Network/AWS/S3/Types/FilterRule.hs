{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.FilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.FilterRule
  ( FilterRule (..),

    -- * Smart constructor
    mkFilterRule,

    -- * Lenses
    frValue,
    frName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.FilterRuleName

-- | Specifies the Amazon S3 object key name to filter on and whether to filter on the suffix or prefix of the key name.
--
-- /See:/ 'mkFilterRule' smart constructor.
data FilterRule = FilterRule'
  { value :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe FilterRuleName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FilterRule' with the minimum fields required to make a request.
--
-- * 'name' - The object key name prefix or suffix identifying one or more objects to which the filtering rule applies. The maximum length is 1,024 characters. Overlapping prefixes and suffixes are not supported. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'value' - The value that the filter searches for in object key names.
mkFilterRule ::
  FilterRule
mkFilterRule =
  FilterRule' {value = Lude.Nothing, name = Lude.Nothing}

-- | The value that the filter searches for in object key names.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frValue :: Lens.Lens' FilterRule (Lude.Maybe Lude.Text)
frValue = Lens.lens (value :: FilterRule -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: FilterRule)
{-# DEPRECATED frValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The object key name prefix or suffix identifying one or more objects to which the filtering rule applies. The maximum length is 1,024 characters. Overlapping prefixes and suffixes are not supported. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frName :: Lens.Lens' FilterRule (Lude.Maybe FilterRuleName)
frName = Lens.lens (name :: FilterRule -> Lude.Maybe FilterRuleName) (\s a -> s {name = a} :: FilterRule)
{-# DEPRECATED frName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML FilterRule where
  parseXML x =
    FilterRule'
      Lude.<$> (x Lude..@? "Value") Lude.<*> (x Lude..@? "Name")

instance Lude.ToXML FilterRule where
  toXML FilterRule' {..} =
    Lude.mconcat ["Value" Lude.@= value, "Name" Lude.@= name]
