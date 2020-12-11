-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Content
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Content
  ( Content (..),

    -- * Smart constructor
    mkContent,

    -- * Lenses
    cCharset,
    cData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents textual data, plus an optional character set specification.
--
-- By default, the text must be 7-bit ASCII, due to the constraints of the SMTP protocol. If the text must contain any other characters, then you must also specify a character set. Examples include UTF-8, ISO-8859-1, and Shift_JIS.
--
-- /See:/ 'mkContent' smart constructor.
data Content = Content'
  { charset :: Lude.Maybe Lude.Text,
    data' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Content' with the minimum fields required to make a request.
--
-- * 'charset' - The character set of the content.
-- * 'data'' - The textual data of the content.
mkContent ::
  -- | 'data''
  Lude.Text ->
  Content
mkContent pData_ = Content' {charset = Lude.Nothing, data' = pData_}

-- | The character set of the content.
--
-- /Note:/ Consider using 'charset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCharset :: Lens.Lens' Content (Lude.Maybe Lude.Text)
cCharset = Lens.lens (charset :: Content -> Lude.Maybe Lude.Text) (\s a -> s {charset = a} :: Content)
{-# DEPRECATED cCharset "Use generic-lens or generic-optics with 'charset' instead." #-}

-- | The textual data of the content.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cData :: Lens.Lens' Content Lude.Text
cData = Lens.lens (data' :: Content -> Lude.Text) (\s a -> s {data' = a} :: Content)
{-# DEPRECATED cData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Lude.ToQuery Content where
  toQuery Content' {..} =
    Lude.mconcat ["Charset" Lude.=: charset, "Data" Lude.=: data']
