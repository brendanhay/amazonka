{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ResourceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResourceRecord
  ( ResourceRecord (..),

    -- * Smart constructor
    mkResourceRecord,

    -- * Lenses
    rrValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

-- | Information specific to the resource record.
--
-- /See:/ 'mkResourceRecord' smart constructor.
newtype ResourceRecord = ResourceRecord' {value :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceRecord' with the minimum fields required to make a request.
--
-- * 'value' - The current or new DNS record value, not to exceed 4,000 characters. In the case of a @DELETE@ action, if the current value does not match the actual value, an error is returned. For descriptions about how to format @Value@ for different record types, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ .
--
-- You can specify more than one value for all record types except @CNAME@ and @SOA@ .
mkResourceRecord ::
  -- | 'value'
  Lude.Text ->
  ResourceRecord
mkResourceRecord pValue_ = ResourceRecord' {value = pValue_}

-- | The current or new DNS record value, not to exceed 4,000 characters. In the case of a @DELETE@ action, if the current value does not match the actual value, an error is returned. For descriptions about how to format @Value@ for different record types, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ .
--
-- You can specify more than one value for all record types except @CNAME@ and @SOA@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRecord Lude.Text
rrValue = Lens.lens (value :: ResourceRecord -> Lude.Text) (\s a -> s {value = a} :: ResourceRecord)
{-# DEPRECATED rrValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML ResourceRecord where
  parseXML x = ResourceRecord' Lude.<$> (x Lude..@ "Value")

instance Lude.ToXML ResourceRecord where
  toXML ResourceRecord' {..} = Lude.mconcat ["Value" Lude.@= value]
