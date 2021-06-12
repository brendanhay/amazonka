{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.SanitizationWarning
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.SanitizationWarning where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.SanitizationWarningReason

-- | Returns information about the XML element or attribute that was
-- sanitized in the configuration.
--
-- /See:/ 'newSanitizationWarning' smart constructor.
data SanitizationWarning = SanitizationWarning'
  { -- | The name of the XML element that has been sanitized.
    elementName :: Core.Maybe Core.Text,
    -- | The name of the XML attribute that has been sanitized.
    attributeName :: Core.Maybe Core.Text,
    -- | Required. The reason for which the XML elements or attributes were
    -- sanitized.
    reason :: Core.Maybe SanitizationWarningReason
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SanitizationWarning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elementName', 'sanitizationWarning_elementName' - The name of the XML element that has been sanitized.
--
-- 'attributeName', 'sanitizationWarning_attributeName' - The name of the XML attribute that has been sanitized.
--
-- 'reason', 'sanitizationWarning_reason' - Required. The reason for which the XML elements or attributes were
-- sanitized.
newSanitizationWarning ::
  SanitizationWarning
newSanitizationWarning =
  SanitizationWarning'
    { elementName = Core.Nothing,
      attributeName = Core.Nothing,
      reason = Core.Nothing
    }

-- | The name of the XML element that has been sanitized.
sanitizationWarning_elementName :: Lens.Lens' SanitizationWarning (Core.Maybe Core.Text)
sanitizationWarning_elementName = Lens.lens (\SanitizationWarning' {elementName} -> elementName) (\s@SanitizationWarning' {} a -> s {elementName = a} :: SanitizationWarning)

-- | The name of the XML attribute that has been sanitized.
sanitizationWarning_attributeName :: Lens.Lens' SanitizationWarning (Core.Maybe Core.Text)
sanitizationWarning_attributeName = Lens.lens (\SanitizationWarning' {attributeName} -> attributeName) (\s@SanitizationWarning' {} a -> s {attributeName = a} :: SanitizationWarning)

-- | Required. The reason for which the XML elements or attributes were
-- sanitized.
sanitizationWarning_reason :: Lens.Lens' SanitizationWarning (Core.Maybe SanitizationWarningReason)
sanitizationWarning_reason = Lens.lens (\SanitizationWarning' {reason} -> reason) (\s@SanitizationWarning' {} a -> s {reason = a} :: SanitizationWarning)

instance Core.FromJSON SanitizationWarning where
  parseJSON =
    Core.withObject
      "SanitizationWarning"
      ( \x ->
          SanitizationWarning'
            Core.<$> (x Core..:? "elementName")
            Core.<*> (x Core..:? "attributeName")
            Core.<*> (x Core..:? "reason")
      )

instance Core.Hashable SanitizationWarning

instance Core.NFData SanitizationWarning
