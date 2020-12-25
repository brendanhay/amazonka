{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.AddHeaderAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.AddHeaderAction
  ( AddHeaderAction (..),

    -- * Smart constructor
    mkAddHeaderAction,

    -- * Lenses
    ahaHeaderName,
    ahaHeaderValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.HeaderName as Types
import qualified Network.AWS.SES.Types.HeaderValue as Types

-- | When included in a receipt rule, this action adds a header to the received email.
--
-- For information about adding a header using a receipt rule, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-add-header.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkAddHeaderAction' smart constructor.
data AddHeaderAction = AddHeaderAction'
  { -- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
    headerName :: Types.HeaderName,
    -- | Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
    headerValue :: Types.HeaderValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddHeaderAction' value with any optional fields omitted.
mkAddHeaderAction ::
  -- | 'headerName'
  Types.HeaderName ->
  -- | 'headerValue'
  Types.HeaderValue ->
  AddHeaderAction
mkAddHeaderAction headerName headerValue =
  AddHeaderAction' {headerName, headerValue}

-- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
--
-- /Note:/ Consider using 'headerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahaHeaderName :: Lens.Lens' AddHeaderAction Types.HeaderName
ahaHeaderName = Lens.field @"headerName"
{-# DEPRECATED ahaHeaderName "Use generic-lens or generic-optics with 'headerName' instead." #-}

-- | Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
--
-- /Note:/ Consider using 'headerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahaHeaderValue :: Lens.Lens' AddHeaderAction Types.HeaderValue
ahaHeaderValue = Lens.field @"headerValue"
{-# DEPRECATED ahaHeaderValue "Use generic-lens or generic-optics with 'headerValue' instead." #-}

instance Core.FromXML AddHeaderAction where
  parseXML x =
    AddHeaderAction'
      Core.<$> (x Core..@ "HeaderName") Core.<*> (x Core..@ "HeaderValue")
