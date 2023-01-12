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
-- Module      : Amazonka.IoT.Types.AuditSuppression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditSuppression where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ResourceIdentifier
import qualified Amazonka.Prelude as Prelude

-- | Filters out specific findings of a Device Defender audit.
--
-- /See:/ 'newAuditSuppression' smart constructor.
data AuditSuppression = AuditSuppression'
  { -- | The description of the audit suppression.
    description :: Prelude.Maybe Prelude.Text,
    -- | The expiration date (epoch timestamp in seconds) that you want the
    -- suppression to adhere to.
    expirationDate :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Prelude.Maybe Prelude.Bool,
    checkName :: Prelude.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'auditSuppression_description' - The description of the audit suppression.
--
-- 'expirationDate', 'auditSuppression_expirationDate' - The expiration date (epoch timestamp in seconds) that you want the
-- suppression to adhere to.
--
-- 'suppressIndefinitely', 'auditSuppression_suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- 'checkName', 'auditSuppression_checkName' - Undocumented member.
--
-- 'resourceIdentifier', 'auditSuppression_resourceIdentifier' - Undocumented member.
newAuditSuppression ::
  -- | 'checkName'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  AuditSuppression
newAuditSuppression pCheckName_ pResourceIdentifier_ =
  AuditSuppression'
    { description = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      suppressIndefinitely = Prelude.Nothing,
      checkName = pCheckName_,
      resourceIdentifier = pResourceIdentifier_
    }

-- | The description of the audit suppression.
auditSuppression_description :: Lens.Lens' AuditSuppression (Prelude.Maybe Prelude.Text)
auditSuppression_description = Lens.lens (\AuditSuppression' {description} -> description) (\s@AuditSuppression' {} a -> s {description = a} :: AuditSuppression)

-- | The expiration date (epoch timestamp in seconds) that you want the
-- suppression to adhere to.
auditSuppression_expirationDate :: Lens.Lens' AuditSuppression (Prelude.Maybe Prelude.UTCTime)
auditSuppression_expirationDate = Lens.lens (\AuditSuppression' {expirationDate} -> expirationDate) (\s@AuditSuppression' {} a -> s {expirationDate = a} :: AuditSuppression) Prelude.. Lens.mapping Data._Time

-- | Indicates whether a suppression should exist indefinitely or not.
auditSuppression_suppressIndefinitely :: Lens.Lens' AuditSuppression (Prelude.Maybe Prelude.Bool)
auditSuppression_suppressIndefinitely = Lens.lens (\AuditSuppression' {suppressIndefinitely} -> suppressIndefinitely) (\s@AuditSuppression' {} a -> s {suppressIndefinitely = a} :: AuditSuppression)

-- | Undocumented member.
auditSuppression_checkName :: Lens.Lens' AuditSuppression Prelude.Text
auditSuppression_checkName = Lens.lens (\AuditSuppression' {checkName} -> checkName) (\s@AuditSuppression' {} a -> s {checkName = a} :: AuditSuppression)

-- | Undocumented member.
auditSuppression_resourceIdentifier :: Lens.Lens' AuditSuppression ResourceIdentifier
auditSuppression_resourceIdentifier = Lens.lens (\AuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@AuditSuppression' {} a -> s {resourceIdentifier = a} :: AuditSuppression)

instance Data.FromJSON AuditSuppression where
  parseJSON =
    Data.withObject
      "AuditSuppression"
      ( \x ->
          AuditSuppression'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "expirationDate")
            Prelude.<*> (x Data..:? "suppressIndefinitely")
            Prelude.<*> (x Data..: "checkName")
            Prelude.<*> (x Data..: "resourceIdentifier")
      )

instance Prelude.Hashable AuditSuppression where
  hashWithSalt _salt AuditSuppression' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expirationDate
      `Prelude.hashWithSalt` suppressIndefinitely
      `Prelude.hashWithSalt` checkName
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData AuditSuppression where
  rnf AuditSuppression' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf suppressIndefinitely
      `Prelude.seq` Prelude.rnf checkName
      `Prelude.seq` Prelude.rnf resourceIdentifier
