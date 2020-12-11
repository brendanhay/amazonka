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
import qualified Network.AWS.Prelude as Lude

-- | When included in a receipt rule, this action adds a header to the received email.
--
-- For information about adding a header using a receipt rule, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-add-header.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkAddHeaderAction' smart constructor.
data AddHeaderAction = AddHeaderAction'
  { headerName :: Lude.Text,
    headerValue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddHeaderAction' with the minimum fields required to make a request.
--
-- * 'headerName' - The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
-- * 'headerValue' - Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
mkAddHeaderAction ::
  -- | 'headerName'
  Lude.Text ->
  -- | 'headerValue'
  Lude.Text ->
  AddHeaderAction
mkAddHeaderAction pHeaderName_ pHeaderValue_ =
  AddHeaderAction'
    { headerName = pHeaderName_,
      headerValue = pHeaderValue_
    }

-- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
--
-- /Note:/ Consider using 'headerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahaHeaderName :: Lens.Lens' AddHeaderAction Lude.Text
ahaHeaderName = Lens.lens (headerName :: AddHeaderAction -> Lude.Text) (\s a -> s {headerName = a} :: AddHeaderAction)
{-# DEPRECATED ahaHeaderName "Use generic-lens or generic-optics with 'headerName' instead." #-}

-- | Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
--
-- /Note:/ Consider using 'headerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahaHeaderValue :: Lens.Lens' AddHeaderAction Lude.Text
ahaHeaderValue = Lens.lens (headerValue :: AddHeaderAction -> Lude.Text) (\s a -> s {headerValue = a} :: AddHeaderAction)
{-# DEPRECATED ahaHeaderValue "Use generic-lens or generic-optics with 'headerValue' instead." #-}

instance Lude.FromXML AddHeaderAction where
  parseXML x =
    AddHeaderAction'
      Lude.<$> (x Lude..@ "HeaderName") Lude.<*> (x Lude..@ "HeaderValue")

instance Lude.ToQuery AddHeaderAction where
  toQuery AddHeaderAction' {..} =
    Lude.mconcat
      [ "HeaderName" Lude.=: headerName,
        "HeaderValue" Lude.=: headerValue
      ]
