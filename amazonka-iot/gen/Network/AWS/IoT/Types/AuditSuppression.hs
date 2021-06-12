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
-- Module      : Network.AWS.IoT.Types.AuditSuppression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditSuppression where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ResourceIdentifier
import qualified Network.AWS.Lens as Lens

-- | Filters out specific findings of a Device Defender audit.
--
-- /See:/ 'newAuditSuppression' smart constructor.
data AuditSuppression = AuditSuppression'
  { -- | The expiration date (epoch timestamp in seconds) that you want the
    -- suppression to adhere to.
    expirationDate :: Core.Maybe Core.POSIX,
    -- | The description of the audit suppression.
    description :: Core.Maybe Core.Text,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Core.Maybe Core.Bool,
    checkName :: Core.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuditSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationDate', 'auditSuppression_expirationDate' - The expiration date (epoch timestamp in seconds) that you want the
-- suppression to adhere to.
--
-- 'description', 'auditSuppression_description' - The description of the audit suppression.
--
-- 'suppressIndefinitely', 'auditSuppression_suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- 'checkName', 'auditSuppression_checkName' - Undocumented member.
--
-- 'resourceIdentifier', 'auditSuppression_resourceIdentifier' - Undocumented member.
newAuditSuppression ::
  -- | 'checkName'
  Core.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  AuditSuppression
newAuditSuppression pCheckName_ pResourceIdentifier_ =
  AuditSuppression'
    { expirationDate = Core.Nothing,
      description = Core.Nothing,
      suppressIndefinitely = Core.Nothing,
      checkName = pCheckName_,
      resourceIdentifier = pResourceIdentifier_
    }

-- | The expiration date (epoch timestamp in seconds) that you want the
-- suppression to adhere to.
auditSuppression_expirationDate :: Lens.Lens' AuditSuppression (Core.Maybe Core.UTCTime)
auditSuppression_expirationDate = Lens.lens (\AuditSuppression' {expirationDate} -> expirationDate) (\s@AuditSuppression' {} a -> s {expirationDate = a} :: AuditSuppression) Core.. Lens.mapping Core._Time

-- | The description of the audit suppression.
auditSuppression_description :: Lens.Lens' AuditSuppression (Core.Maybe Core.Text)
auditSuppression_description = Lens.lens (\AuditSuppression' {description} -> description) (\s@AuditSuppression' {} a -> s {description = a} :: AuditSuppression)

-- | Indicates whether a suppression should exist indefinitely or not.
auditSuppression_suppressIndefinitely :: Lens.Lens' AuditSuppression (Core.Maybe Core.Bool)
auditSuppression_suppressIndefinitely = Lens.lens (\AuditSuppression' {suppressIndefinitely} -> suppressIndefinitely) (\s@AuditSuppression' {} a -> s {suppressIndefinitely = a} :: AuditSuppression)

-- | Undocumented member.
auditSuppression_checkName :: Lens.Lens' AuditSuppression Core.Text
auditSuppression_checkName = Lens.lens (\AuditSuppression' {checkName} -> checkName) (\s@AuditSuppression' {} a -> s {checkName = a} :: AuditSuppression)

-- | Undocumented member.
auditSuppression_resourceIdentifier :: Lens.Lens' AuditSuppression ResourceIdentifier
auditSuppression_resourceIdentifier = Lens.lens (\AuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@AuditSuppression' {} a -> s {resourceIdentifier = a} :: AuditSuppression)

instance Core.FromJSON AuditSuppression where
  parseJSON =
    Core.withObject
      "AuditSuppression"
      ( \x ->
          AuditSuppression'
            Core.<$> (x Core..:? "expirationDate")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "suppressIndefinitely")
            Core.<*> (x Core..: "checkName")
            Core.<*> (x Core..: "resourceIdentifier")
      )

instance Core.Hashable AuditSuppression

instance Core.NFData AuditSuppression
