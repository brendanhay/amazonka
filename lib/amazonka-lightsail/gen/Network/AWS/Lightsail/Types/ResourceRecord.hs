{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceRecord
  ( ResourceRecord (..),

    -- * Smart constructor
    mkResourceRecord,

    -- * Lenses
    rrValue,
    rrName,
    rrType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the domain name system (DNS) records to add to your domain's DNS to validate it for an Amazon Lightsail certificate.
--
-- /See:/ 'mkResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { value ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceRecord' with the minimum fields required to make a request.
--
-- * 'name' - The name of the record.
-- * 'type'' - The DNS record type.
-- * 'value' - The value for the DNS record.
mkResourceRecord ::
  ResourceRecord
mkResourceRecord =
  ResourceRecord'
    { value = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The value for the DNS record.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrValue :: Lens.Lens' ResourceRecord (Lude.Maybe Lude.Text)
rrValue = Lens.lens (value :: ResourceRecord -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ResourceRecord)
{-# DEPRECATED rrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the record.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrName :: Lens.Lens' ResourceRecord (Lude.Maybe Lude.Text)
rrName = Lens.lens (name :: ResourceRecord -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ResourceRecord)
{-# DEPRECATED rrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The DNS record type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ResourceRecord (Lude.Maybe Lude.Text)
rrType = Lens.lens (type' :: ResourceRecord -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ResourceRecord)
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ResourceRecord where
  parseJSON =
    Lude.withObject
      "ResourceRecord"
      ( \x ->
          ResourceRecord'
            Lude.<$> (x Lude..:? "value")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "type")
      )
