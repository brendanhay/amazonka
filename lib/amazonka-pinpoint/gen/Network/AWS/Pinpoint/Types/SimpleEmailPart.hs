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
    sepData,
    sepCharset,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the subject or body of an email message, represented as textual email data and the applicable character set.
--
-- /See:/ 'mkSimpleEmailPart' smart constructor.
data SimpleEmailPart = SimpleEmailPart'
  { data' ::
      Lude.Maybe Lude.Text,
    charset :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SimpleEmailPart' with the minimum fields required to make a request.
--
-- * 'charset' - The applicable character set for the message content.
-- * 'data'' - The textual data of the message content.
mkSimpleEmailPart ::
  SimpleEmailPart
mkSimpleEmailPart =
  SimpleEmailPart' {data' = Lude.Nothing, charset = Lude.Nothing}

-- | The textual data of the message content.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sepData :: Lens.Lens' SimpleEmailPart (Lude.Maybe Lude.Text)
sepData = Lens.lens (data' :: SimpleEmailPart -> Lude.Maybe Lude.Text) (\s a -> s {data' = a} :: SimpleEmailPart)
{-# DEPRECATED sepData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The applicable character set for the message content.
--
-- /Note:/ Consider using 'charset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sepCharset :: Lens.Lens' SimpleEmailPart (Lude.Maybe Lude.Text)
sepCharset = Lens.lens (charset :: SimpleEmailPart -> Lude.Maybe Lude.Text) (\s a -> s {charset = a} :: SimpleEmailPart)
{-# DEPRECATED sepCharset "Use generic-lens or generic-optics with 'charset' instead." #-}

instance Lude.ToJSON SimpleEmailPart where
  toJSON SimpleEmailPart' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Data" Lude..=) Lude.<$> data',
            ("Charset" Lude..=) Lude.<$> charset
          ]
      )
