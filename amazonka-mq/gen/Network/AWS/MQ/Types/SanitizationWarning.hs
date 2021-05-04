{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.SanitizationWarningReason
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about the XML element or attribute that was
-- sanitized in the configuration.
--
-- /See:/ 'newSanitizationWarning' smart constructor.
data SanitizationWarning = SanitizationWarning'
  { -- | The name of the XML element that has been sanitized.
    elementName :: Prelude.Maybe Prelude.Text,
    -- | The name of the XML attribute that has been sanitized.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | Required. The reason for which the XML elements or attributes were
    -- sanitized.
    reason :: Prelude.Maybe SanitizationWarningReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { elementName = Prelude.Nothing,
      attributeName = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The name of the XML element that has been sanitized.
sanitizationWarning_elementName :: Lens.Lens' SanitizationWarning (Prelude.Maybe Prelude.Text)
sanitizationWarning_elementName = Lens.lens (\SanitizationWarning' {elementName} -> elementName) (\s@SanitizationWarning' {} a -> s {elementName = a} :: SanitizationWarning)

-- | The name of the XML attribute that has been sanitized.
sanitizationWarning_attributeName :: Lens.Lens' SanitizationWarning (Prelude.Maybe Prelude.Text)
sanitizationWarning_attributeName = Lens.lens (\SanitizationWarning' {attributeName} -> attributeName) (\s@SanitizationWarning' {} a -> s {attributeName = a} :: SanitizationWarning)

-- | Required. The reason for which the XML elements or attributes were
-- sanitized.
sanitizationWarning_reason :: Lens.Lens' SanitizationWarning (Prelude.Maybe SanitizationWarningReason)
sanitizationWarning_reason = Lens.lens (\SanitizationWarning' {reason} -> reason) (\s@SanitizationWarning' {} a -> s {reason = a} :: SanitizationWarning)

instance Prelude.FromJSON SanitizationWarning where
  parseJSON =
    Prelude.withObject
      "SanitizationWarning"
      ( \x ->
          SanitizationWarning'
            Prelude.<$> (x Prelude..:? "elementName")
            Prelude.<*> (x Prelude..:? "attributeName")
            Prelude.<*> (x Prelude..:? "reason")
      )

instance Prelude.Hashable SanitizationWarning

instance Prelude.NFData SanitizationWarning
