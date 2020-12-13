{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PartnerEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PartnerEventSource
  ( PartnerEventSource (..),

    -- * Smart constructor
    mkPartnerEventSource,

    -- * Lenses
    pesARN,
    pesName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A partner event source is created by an SaaS partner. If a customer creates a partner event bus that matches this event source, that AWS account can receive events from the partner's applications or services.
--
-- /See:/ 'mkPartnerEventSource' smart constructor.
data PartnerEventSource = PartnerEventSource'
  { -- | The ARN of the partner event source.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the partner event source.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartnerEventSource' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the partner event source.
-- * 'name' - The name of the partner event source.
mkPartnerEventSource ::
  PartnerEventSource
mkPartnerEventSource =
  PartnerEventSource' {arn = Lude.Nothing, name = Lude.Nothing}

-- | The ARN of the partner event source.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesARN :: Lens.Lens' PartnerEventSource (Lude.Maybe Lude.Text)
pesARN = Lens.lens (arn :: PartnerEventSource -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PartnerEventSource)
{-# DEPRECATED pesARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the partner event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesName :: Lens.Lens' PartnerEventSource (Lude.Maybe Lude.Text)
pesName = Lens.lens (name :: PartnerEventSource -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PartnerEventSource)
{-# DEPRECATED pesName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON PartnerEventSource where
  parseJSON =
    Lude.withObject
      "PartnerEventSource"
      ( \x ->
          PartnerEventSource'
            Lude.<$> (x Lude..:? "Arn") Lude.<*> (x Lude..:? "Name")
      )
