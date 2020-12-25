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
    efName,
    efValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.ExtensionFieldName as Types
import qualified Network.AWS.SES.Types.ExtensionFieldValue as Types

-- | Additional X-headers to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkExtensionField' smart constructor.
data ExtensionField = ExtensionField'
  { -- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
    name :: Types.ExtensionFieldName,
    -- | The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
    value :: Types.ExtensionFieldValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExtensionField' value with any optional fields omitted.
mkExtensionField ::
  -- | 'name'
  Types.ExtensionFieldName ->
  -- | 'value'
  Types.ExtensionFieldValue ->
  ExtensionField
mkExtensionField name value = ExtensionField' {name, value}

-- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efName :: Lens.Lens' ExtensionField Types.ExtensionFieldName
efName = Lens.field @"name"
{-# DEPRECATED efName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efValue :: Lens.Lens' ExtensionField Types.ExtensionFieldValue
efValue = Lens.field @"value"
{-# DEPRECATED efValue "Use generic-lens or generic-optics with 'value' instead." #-}
