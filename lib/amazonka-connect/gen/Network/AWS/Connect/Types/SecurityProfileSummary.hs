{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.SecurityProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.SecurityProfileSummary
  ( SecurityProfileSummary (..),

    -- * Smart constructor
    mkSecurityProfileSummary,

    -- * Lenses
    spsARN,
    spsName,
    spsId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a security profile.
--
-- /See:/ 'mkSecurityProfileSummary' smart constructor.
data SecurityProfileSummary = SecurityProfileSummary'
  { arn ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityProfileSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the security profile.
-- * 'id' - The identifier of the security profile.
-- * 'name' - The name of the security profile.
mkSecurityProfileSummary ::
  SecurityProfileSummary
mkSecurityProfileSummary =
  SecurityProfileSummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the security profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsARN :: Lens.Lens' SecurityProfileSummary (Lude.Maybe Lude.Text)
spsARN = Lens.lens (arn :: SecurityProfileSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: SecurityProfileSummary)
{-# DEPRECATED spsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the security profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsName :: Lens.Lens' SecurityProfileSummary (Lude.Maybe Lude.Text)
spsName = Lens.lens (name :: SecurityProfileSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SecurityProfileSummary)
{-# DEPRECATED spsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the security profile.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsId :: Lens.Lens' SecurityProfileSummary (Lude.Maybe Lude.Text)
spsId = Lens.lens (id :: SecurityProfileSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: SecurityProfileSummary)
{-# DEPRECATED spsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON SecurityProfileSummary where
  parseJSON =
    Lude.withObject
      "SecurityProfileSummary"
      ( \x ->
          SecurityProfileSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
