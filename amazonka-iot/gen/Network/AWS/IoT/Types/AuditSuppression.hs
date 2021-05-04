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
-- Module      : Network.AWS.IoT.Types.AuditSuppression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditSuppression where

import Network.AWS.IoT.Types.ResourceIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filters out specific findings of a Device Defender audit.
--
-- /See:/ 'newAuditSuppression' smart constructor.
data AuditSuppression = AuditSuppression'
  { -- | The expiration date (epoch timestamp in seconds) that you want the
    -- suppression to adhere to.
    expirationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The description of the audit suppression.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Prelude.Maybe Prelude.Bool,
    checkName :: Prelude.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  AuditSuppression
newAuditSuppression pCheckName_ pResourceIdentifier_ =
  AuditSuppression'
    { expirationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      suppressIndefinitely = Prelude.Nothing,
      checkName = pCheckName_,
      resourceIdentifier = pResourceIdentifier_
    }

-- | The expiration date (epoch timestamp in seconds) that you want the
-- suppression to adhere to.
auditSuppression_expirationDate :: Lens.Lens' AuditSuppression (Prelude.Maybe Prelude.UTCTime)
auditSuppression_expirationDate = Lens.lens (\AuditSuppression' {expirationDate} -> expirationDate) (\s@AuditSuppression' {} a -> s {expirationDate = a} :: AuditSuppression) Prelude.. Lens.mapping Prelude._Time

-- | The description of the audit suppression.
auditSuppression_description :: Lens.Lens' AuditSuppression (Prelude.Maybe Prelude.Text)
auditSuppression_description = Lens.lens (\AuditSuppression' {description} -> description) (\s@AuditSuppression' {} a -> s {description = a} :: AuditSuppression)

-- | Indicates whether a suppression should exist indefinitely or not.
auditSuppression_suppressIndefinitely :: Lens.Lens' AuditSuppression (Prelude.Maybe Prelude.Bool)
auditSuppression_suppressIndefinitely = Lens.lens (\AuditSuppression' {suppressIndefinitely} -> suppressIndefinitely) (\s@AuditSuppression' {} a -> s {suppressIndefinitely = a} :: AuditSuppression)

-- | Undocumented member.
auditSuppression_checkName :: Lens.Lens' AuditSuppression Prelude.Text
auditSuppression_checkName = Lens.lens (\AuditSuppression' {checkName} -> checkName) (\s@AuditSuppression' {} a -> s {checkName = a} :: AuditSuppression)

-- | Undocumented member.
auditSuppression_resourceIdentifier :: Lens.Lens' AuditSuppression ResourceIdentifier
auditSuppression_resourceIdentifier = Lens.lens (\AuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@AuditSuppression' {} a -> s {resourceIdentifier = a} :: AuditSuppression)

instance Prelude.FromJSON AuditSuppression where
  parseJSON =
    Prelude.withObject
      "AuditSuppression"
      ( \x ->
          AuditSuppression'
            Prelude.<$> (x Prelude..:? "expirationDate")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "suppressIndefinitely")
            Prelude.<*> (x Prelude..: "checkName")
            Prelude.<*> (x Prelude..: "resourceIdentifier")
      )

instance Prelude.Hashable AuditSuppression

instance Prelude.NFData AuditSuppression
