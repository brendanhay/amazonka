{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cData,
    cCharset,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.Charset as Types
import qualified Network.AWS.SES.Types.Data as Types

-- | Represents textual data, plus an optional character set specification.
--
-- By default, the text must be 7-bit ASCII, due to the constraints of the SMTP protocol. If the text must contain any other characters, then you must also specify a character set. Examples include UTF-8, ISO-8859-1, and Shift_JIS.
--
-- /See:/ 'mkContent' smart constructor.
data Content = Content'
  { -- | The textual data of the content.
    data' :: Types.Data,
    -- | The character set of the content.
    charset :: Core.Maybe Types.Charset
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Content' value with any optional fields omitted.
mkContent ::
  -- | 'data\''
  Types.Data ->
  Content
mkContent data' = Content' {data', charset = Core.Nothing}

-- | The textual data of the content.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cData :: Lens.Lens' Content Types.Data
cData = Lens.field @"data'"
{-# DEPRECATED cData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The character set of the content.
--
-- /Note:/ Consider using 'charset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCharset :: Lens.Lens' Content (Core.Maybe Types.Charset)
cCharset = Lens.field @"charset"
{-# DEPRECATED cCharset "Use generic-lens or generic-optics with 'charset' instead." #-}
