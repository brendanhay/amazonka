{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileSummary
  ( RoutingProfileSummary (..),

    -- * Smart constructor
    mkRoutingProfileSummary,

    -- * Lenses
    rpsARN,
    rpsName,
    rpsId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a routing profile.
--
-- /See:/ 'mkRoutingProfileSummary' smart constructor.
data RoutingProfileSummary = RoutingProfileSummary'
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

-- | Creates a value of 'RoutingProfileSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the routing profile.
-- * 'id' - The identifier of the routing profile.
-- * 'name' - The name of the routing profile.
mkRoutingProfileSummary ::
  RoutingProfileSummary
mkRoutingProfileSummary =
  RoutingProfileSummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the routing profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsARN :: Lens.Lens' RoutingProfileSummary (Lude.Maybe Lude.Text)
rpsARN = Lens.lens (arn :: RoutingProfileSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RoutingProfileSummary)
{-# DEPRECATED rpsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the routing profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsName :: Lens.Lens' RoutingProfileSummary (Lude.Maybe Lude.Text)
rpsName = Lens.lens (name :: RoutingProfileSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RoutingProfileSummary)
{-# DEPRECATED rpsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsId :: Lens.Lens' RoutingProfileSummary (Lude.Maybe Lude.Text)
rpsId = Lens.lens (id :: RoutingProfileSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: RoutingProfileSummary)
{-# DEPRECATED rpsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON RoutingProfileSummary where
  parseJSON =
    Lude.withObject
      "RoutingProfileSummary"
      ( \x ->
          RoutingProfileSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
