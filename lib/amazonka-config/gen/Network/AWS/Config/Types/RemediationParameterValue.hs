-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationParameterValue
  ( RemediationParameterValue (..),

    -- * Smart constructor
    mkRemediationParameterValue,

    -- * Lenses
    rpvStaticValue,
    rpvResourceValue,
  )
where

import Network.AWS.Config.Types.ResourceValue
import Network.AWS.Config.Types.StaticValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The value is either a dynamic (resource) value or a static value. You must select either a dynamic value or a static value.
--
-- /See:/ 'mkRemediationParameterValue' smart constructor.
data RemediationParameterValue = RemediationParameterValue'
  { staticValue ::
      Lude.Maybe StaticValue,
    resourceValue ::
      Lude.Maybe ResourceValue
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemediationParameterValue' with the minimum fields required to make a request.
--
-- * 'resourceValue' - The value is dynamic and changes at run-time.
-- * 'staticValue' - The value is static and does not change at run-time.
mkRemediationParameterValue ::
  RemediationParameterValue
mkRemediationParameterValue =
  RemediationParameterValue'
    { staticValue = Lude.Nothing,
      resourceValue = Lude.Nothing
    }

-- | The value is static and does not change at run-time.
--
-- /Note:/ Consider using 'staticValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpvStaticValue :: Lens.Lens' RemediationParameterValue (Lude.Maybe StaticValue)
rpvStaticValue = Lens.lens (staticValue :: RemediationParameterValue -> Lude.Maybe StaticValue) (\s a -> s {staticValue = a} :: RemediationParameterValue)
{-# DEPRECATED rpvStaticValue "Use generic-lens or generic-optics with 'staticValue' instead." #-}

-- | The value is dynamic and changes at run-time.
--
-- /Note:/ Consider using 'resourceValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpvResourceValue :: Lens.Lens' RemediationParameterValue (Lude.Maybe ResourceValue)
rpvResourceValue = Lens.lens (resourceValue :: RemediationParameterValue -> Lude.Maybe ResourceValue) (\s a -> s {resourceValue = a} :: RemediationParameterValue)
{-# DEPRECATED rpvResourceValue "Use generic-lens or generic-optics with 'resourceValue' instead." #-}

instance Lude.FromJSON RemediationParameterValue where
  parseJSON =
    Lude.withObject
      "RemediationParameterValue"
      ( \x ->
          RemediationParameterValue'
            Lude.<$> (x Lude..:? "StaticValue") Lude.<*> (x Lude..:? "ResourceValue")
      )

instance Lude.ToJSON RemediationParameterValue where
  toJSON RemediationParameterValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StaticValue" Lude..=) Lude.<$> staticValue,
            ("ResourceValue" Lude..=) Lude.<$> resourceValue
          ]
      )
