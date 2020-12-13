{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ServiceId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ServiceId
  ( ServiceId (..),

    -- * Smart constructor
    mkServiceId,

    -- * Lenses
    siAccountId,
    siNames,
    siName,
    siType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- |
--
-- /See:/ 'mkServiceId' smart constructor.
data ServiceId = ServiceId'
  { -- |
    accountId :: Lude.Maybe Lude.Text,
    -- |
    names :: Lude.Maybe [Lude.Text],
    -- |
    name :: Lude.Maybe Lude.Text,
    -- |
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceId' with the minimum fields required to make a request.
--
-- * 'accountId' -
-- * 'names' -
-- * 'name' -
-- * 'type'' -
mkServiceId ::
  ServiceId
mkServiceId =
  ServiceId'
    { accountId = Lude.Nothing,
      names = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAccountId :: Lens.Lens' ServiceId (Lude.Maybe Lude.Text)
siAccountId = Lens.lens (accountId :: ServiceId -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ServiceId)
{-# DEPRECATED siAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- |
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNames :: Lens.Lens' ServiceId (Lude.Maybe [Lude.Text])
siNames = Lens.lens (names :: ServiceId -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: ServiceId)
{-# DEPRECATED siNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- |
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siName :: Lens.Lens' ServiceId (Lude.Maybe Lude.Text)
siName = Lens.lens (name :: ServiceId -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ServiceId)
{-# DEPRECATED siName "Use generic-lens or generic-optics with 'name' instead." #-}

-- |
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siType :: Lens.Lens' ServiceId (Lude.Maybe Lude.Text)
siType = Lens.lens (type' :: ServiceId -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ServiceId)
{-# DEPRECATED siType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ServiceId where
  parseJSON =
    Lude.withObject
      "ServiceId"
      ( \x ->
          ServiceId'
            Lude.<$> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "Names" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Type")
      )
