{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.SanitizationWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.SanitizationWarning
  ( SanitizationWarning (..),

    -- * Smart constructor
    mkSanitizationWarning,

    -- * Lenses
    swReason,
    swAttributeName,
    swElementName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.SanitizationWarningReason
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the XML element or attribute that was sanitized in the configuration.
--
-- /See:/ 'mkSanitizationWarning' smart constructor.
data SanitizationWarning = SanitizationWarning'
  { reason ::
      Lude.Maybe SanitizationWarningReason,
    attributeName :: Lude.Maybe Lude.Text,
    elementName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SanitizationWarning' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the XML attribute that has been sanitized.
-- * 'elementName' - The name of the XML element that has been sanitized.
-- * 'reason' - Required. The reason for which the XML elements or attributes were sanitized.
mkSanitizationWarning ::
  SanitizationWarning
mkSanitizationWarning =
  SanitizationWarning'
    { reason = Lude.Nothing,
      attributeName = Lude.Nothing,
      elementName = Lude.Nothing
    }

-- | Required. The reason for which the XML elements or attributes were sanitized.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swReason :: Lens.Lens' SanitizationWarning (Lude.Maybe SanitizationWarningReason)
swReason = Lens.lens (reason :: SanitizationWarning -> Lude.Maybe SanitizationWarningReason) (\s a -> s {reason = a} :: SanitizationWarning)
{-# DEPRECATED swReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The name of the XML attribute that has been sanitized.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swAttributeName :: Lens.Lens' SanitizationWarning (Lude.Maybe Lude.Text)
swAttributeName = Lens.lens (attributeName :: SanitizationWarning -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: SanitizationWarning)
{-# DEPRECATED swAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The name of the XML element that has been sanitized.
--
-- /Note:/ Consider using 'elementName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swElementName :: Lens.Lens' SanitizationWarning (Lude.Maybe Lude.Text)
swElementName = Lens.lens (elementName :: SanitizationWarning -> Lude.Maybe Lude.Text) (\s a -> s {elementName = a} :: SanitizationWarning)
{-# DEPRECATED swElementName "Use generic-lens or generic-optics with 'elementName' instead." #-}

instance Lude.FromJSON SanitizationWarning where
  parseJSON =
    Lude.withObject
      "SanitizationWarning"
      ( \x ->
          SanitizationWarning'
            Lude.<$> (x Lude..:? "reason")
            Lude.<*> (x Lude..:? "attributeName")
            Lude.<*> (x Lude..:? "elementName")
      )
