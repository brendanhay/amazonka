-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ExtensionField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ExtensionField
  ( ExtensionField (..),

    -- * Smart constructor
    mkExtensionField,

    -- * Lenses
    efName,
    efValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Additional X-headers to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkExtensionField' smart constructor.
data ExtensionField = ExtensionField'
  { name :: Lude.Text,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExtensionField' with the minimum fields required to make a request.
--
-- * 'name' - The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
-- * 'value' - The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
mkExtensionField ::
  -- | 'name'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  ExtensionField
mkExtensionField pName_ pValue_ =
  ExtensionField' {name = pName_, value = pValue_}

-- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efName :: Lens.Lens' ExtensionField Lude.Text
efName = Lens.lens (name :: ExtensionField -> Lude.Text) (\s a -> s {name = a} :: ExtensionField)
{-# DEPRECATED efName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efValue :: Lens.Lens' ExtensionField Lude.Text
efValue = Lens.lens (value :: ExtensionField -> Lude.Text) (\s a -> s {value = a} :: ExtensionField)
{-# DEPRECATED efValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery ExtensionField where
  toQuery ExtensionField' {..} =
    Lude.mconcat ["Name" Lude.=: name, "Value" Lude.=: value]
