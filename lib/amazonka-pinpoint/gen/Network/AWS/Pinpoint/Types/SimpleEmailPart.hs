{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SimpleEmailPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SimpleEmailPart
  ( SimpleEmailPart (..),

    -- * Smart constructor
    mkSimpleEmailPart,

    -- * Lenses
    sepCharset,
    sepData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the subject or body of an email message, represented as textual email data and the applicable character set.
--
-- /See:/ 'mkSimpleEmailPart' smart constructor.
data SimpleEmailPart = SimpleEmailPart'
  { -- | The applicable character set for the message content.
    charset :: Core.Maybe Core.Text,
    -- | The textual data of the message content.
    data' :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SimpleEmailPart' value with any optional fields omitted.
mkSimpleEmailPart ::
  SimpleEmailPart
mkSimpleEmailPart =
  SimpleEmailPart' {charset = Core.Nothing, data' = Core.Nothing}

-- | The applicable character set for the message content.
--
-- /Note:/ Consider using 'charset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sepCharset :: Lens.Lens' SimpleEmailPart (Core.Maybe Core.Text)
sepCharset = Lens.field @"charset"
{-# DEPRECATED sepCharset "Use generic-lens or generic-optics with 'charset' instead." #-}

-- | The textual data of the message content.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sepData :: Lens.Lens' SimpleEmailPart (Core.Maybe Core.Text)
sepData = Lens.field @"data'"
{-# DEPRECATED sepData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Core.FromJSON SimpleEmailPart where
  toJSON SimpleEmailPart {..} =
    Core.object
      ( Core.catMaybes
          [ ("Charset" Core..=) Core.<$> charset,
            ("Data" Core..=) Core.<$> data'
          ]
      )
