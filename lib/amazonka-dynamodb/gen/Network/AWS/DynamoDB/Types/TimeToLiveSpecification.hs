-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveSpecification
  ( TimeToLiveSpecification (..),

    -- * Smart constructor
    mkTimeToLiveSpecification,

    -- * Lenses
    ttlsEnabled,
    ttlsAttributeName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the settings used to enable or disable Time to Live (TTL) for the specified table.
--
-- /See:/ 'mkTimeToLiveSpecification' smart constructor.
data TimeToLiveSpecification = TimeToLiveSpecification'
  { enabled ::
      Lude.Bool,
    attributeName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeToLiveSpecification' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the TTL attribute used to store the expiration time for items in the table.
-- * 'enabled' - Indicates whether TTL is to be enabled (true) or disabled (false) on the table.
mkTimeToLiveSpecification ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 'attributeName'
  Lude.Text ->
  TimeToLiveSpecification
mkTimeToLiveSpecification pEnabled_ pAttributeName_ =
  TimeToLiveSpecification'
    { enabled = pEnabled_,
      attributeName = pAttributeName_
    }

-- | Indicates whether TTL is to be enabled (true) or disabled (false) on the table.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttlsEnabled :: Lens.Lens' TimeToLiveSpecification Lude.Bool
ttlsEnabled = Lens.lens (enabled :: TimeToLiveSpecification -> Lude.Bool) (\s a -> s {enabled = a} :: TimeToLiveSpecification)
{-# DEPRECATED ttlsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The name of the TTL attribute used to store the expiration time for items in the table.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttlsAttributeName :: Lens.Lens' TimeToLiveSpecification Lude.Text
ttlsAttributeName = Lens.lens (attributeName :: TimeToLiveSpecification -> Lude.Text) (\s a -> s {attributeName = a} :: TimeToLiveSpecification)
{-# DEPRECATED ttlsAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromJSON TimeToLiveSpecification where
  parseJSON =
    Lude.withObject
      "TimeToLiveSpecification"
      ( \x ->
          TimeToLiveSpecification'
            Lude.<$> (x Lude..: "Enabled") Lude.<*> (x Lude..: "AttributeName")
      )

instance Lude.ToJSON TimeToLiveSpecification where
  toJSON TimeToLiveSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Enabled" Lude..= enabled),
            Lude.Just ("AttributeName" Lude..= attributeName)
          ]
      )
