-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.AdditionalAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.AdditionalAttribute
  ( AdditionalAttribute (..),

    -- * Smart constructor
    mkAdditionalAttribute,

    -- * Lenses
    aaValue,
    aaKey,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about additional load balancer attributes.
--
-- /See:/ 'mkAdditionalAttribute' smart constructor.
data AdditionalAttribute = AdditionalAttribute'
  { value ::
      Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdditionalAttribute' with the minimum fields required to make a request.
--
-- * 'key' - The name of the attribute.
--
-- The following attribute is supported.
--
--     * @elb.http.desyncmitigationmode@ - Determines how the load balancer handles requests that might pose a security risk to your application. The possible values are @monitor@ , @defensive@ , and @strictest@ . The default is @defensive@ .
--
--
-- * 'value' - This value of the attribute.
mkAdditionalAttribute ::
  AdditionalAttribute
mkAdditionalAttribute =
  AdditionalAttribute' {value = Lude.Nothing, key = Lude.Nothing}

-- | This value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaValue :: Lens.Lens' AdditionalAttribute (Lude.Maybe Lude.Text)
aaValue = Lens.lens (value :: AdditionalAttribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: AdditionalAttribute)
{-# DEPRECATED aaValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the attribute.
--
-- The following attribute is supported.
--
--     * @elb.http.desyncmitigationmode@ - Determines how the load balancer handles requests that might pose a security risk to your application. The possible values are @monitor@ , @defensive@ , and @strictest@ . The default is @defensive@ .
--
--
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaKey :: Lens.Lens' AdditionalAttribute (Lude.Maybe Lude.Text)
aaKey = Lens.lens (key :: AdditionalAttribute -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: AdditionalAttribute)
{-# DEPRECATED aaKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML AdditionalAttribute where
  parseXML x =
    AdditionalAttribute'
      Lude.<$> (x Lude..@? "Value") Lude.<*> (x Lude..@? "Key")

instance Lude.ToQuery AdditionalAttribute where
  toQuery AdditionalAttribute' {..} =
    Lude.mconcat ["Value" Lude.=: value, "Key" Lude.=: key]
