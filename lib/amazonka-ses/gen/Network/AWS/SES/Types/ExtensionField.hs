{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    efValue,
    efName,
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
  { -- | The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
    value :: Lude.Text,
    -- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExtensionField' with the minimum fields required to make a request.
--
-- * 'value' - The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
-- * 'name' - The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
mkExtensionField ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ExtensionField
mkExtensionField pValue_ pName_ =
  ExtensionField' {value = pValue_, name = pName_}

-- | The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efValue :: Lens.Lens' ExtensionField Lude.Text
efValue = Lens.lens (value :: ExtensionField -> Lude.Text) (\s a -> s {value = a} :: ExtensionField)
{-# DEPRECATED efValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efName :: Lens.Lens' ExtensionField Lude.Text
efName = Lens.lens (name :: ExtensionField -> Lude.Text) (\s a -> s {name = a} :: ExtensionField)
{-# DEPRECATED efName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToQuery ExtensionField where
  toQuery ExtensionField' {..} =
    Lude.mconcat ["Value" Lude.=: value, "Name" Lude.=: name]
