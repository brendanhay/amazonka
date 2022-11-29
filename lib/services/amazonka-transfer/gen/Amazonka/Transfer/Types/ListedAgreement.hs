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
-- Module      : Amazonka.Transfer.Types.ListedAgreement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedAgreement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.AgreementStatusType

-- | Describes the properties of an agreement.
--
-- /See:/ 'newListedAgreement' smart constructor.
data ListedAgreement = ListedAgreement'
  { -- | The Amazon Resource Name (ARN) of the specified agreement.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The agreement can be either @ACTIVE@ or @INACTIVE@.
    status :: Prelude.Maybe AgreementStatusType,
    -- | The current description for the agreement. You can change it by calling
    -- the @UpdateAgreement@ operation and providing a new description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the AS2 local profile.
    localProfileId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the agreement. This identifier is returned when
    -- you create an agreement.
    agreementId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the agreement.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the partner profile.
    partnerProfileId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedAgreement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listedAgreement_arn' - The Amazon Resource Name (ARN) of the specified agreement.
--
-- 'status', 'listedAgreement_status' - The agreement can be either @ACTIVE@ or @INACTIVE@.
--
-- 'description', 'listedAgreement_description' - The current description for the agreement. You can change it by calling
-- the @UpdateAgreement@ operation and providing a new description.
--
-- 'localProfileId', 'listedAgreement_localProfileId' - A unique identifier for the AS2 local profile.
--
-- 'agreementId', 'listedAgreement_agreementId' - A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
--
-- 'serverId', 'listedAgreement_serverId' - The unique identifier for the agreement.
--
-- 'partnerProfileId', 'listedAgreement_partnerProfileId' - A unique identifier for the partner profile.
newListedAgreement ::
  ListedAgreement
newListedAgreement =
  ListedAgreement'
    { arn = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      localProfileId = Prelude.Nothing,
      agreementId = Prelude.Nothing,
      serverId = Prelude.Nothing,
      partnerProfileId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the specified agreement.
listedAgreement_arn :: Lens.Lens' ListedAgreement (Prelude.Maybe Prelude.Text)
listedAgreement_arn = Lens.lens (\ListedAgreement' {arn} -> arn) (\s@ListedAgreement' {} a -> s {arn = a} :: ListedAgreement)

-- | The agreement can be either @ACTIVE@ or @INACTIVE@.
listedAgreement_status :: Lens.Lens' ListedAgreement (Prelude.Maybe AgreementStatusType)
listedAgreement_status = Lens.lens (\ListedAgreement' {status} -> status) (\s@ListedAgreement' {} a -> s {status = a} :: ListedAgreement)

-- | The current description for the agreement. You can change it by calling
-- the @UpdateAgreement@ operation and providing a new description.
listedAgreement_description :: Lens.Lens' ListedAgreement (Prelude.Maybe Prelude.Text)
listedAgreement_description = Lens.lens (\ListedAgreement' {description} -> description) (\s@ListedAgreement' {} a -> s {description = a} :: ListedAgreement)

-- | A unique identifier for the AS2 local profile.
listedAgreement_localProfileId :: Lens.Lens' ListedAgreement (Prelude.Maybe Prelude.Text)
listedAgreement_localProfileId = Lens.lens (\ListedAgreement' {localProfileId} -> localProfileId) (\s@ListedAgreement' {} a -> s {localProfileId = a} :: ListedAgreement)

-- | A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
listedAgreement_agreementId :: Lens.Lens' ListedAgreement (Prelude.Maybe Prelude.Text)
listedAgreement_agreementId = Lens.lens (\ListedAgreement' {agreementId} -> agreementId) (\s@ListedAgreement' {} a -> s {agreementId = a} :: ListedAgreement)

-- | The unique identifier for the agreement.
listedAgreement_serverId :: Lens.Lens' ListedAgreement (Prelude.Maybe Prelude.Text)
listedAgreement_serverId = Lens.lens (\ListedAgreement' {serverId} -> serverId) (\s@ListedAgreement' {} a -> s {serverId = a} :: ListedAgreement)

-- | A unique identifier for the partner profile.
listedAgreement_partnerProfileId :: Lens.Lens' ListedAgreement (Prelude.Maybe Prelude.Text)
listedAgreement_partnerProfileId = Lens.lens (\ListedAgreement' {partnerProfileId} -> partnerProfileId) (\s@ListedAgreement' {} a -> s {partnerProfileId = a} :: ListedAgreement)

instance Core.FromJSON ListedAgreement where
  parseJSON =
    Core.withObject
      "ListedAgreement"
      ( \x ->
          ListedAgreement'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LocalProfileId")
            Prelude.<*> (x Core..:? "AgreementId")
            Prelude.<*> (x Core..:? "ServerId")
            Prelude.<*> (x Core..:? "PartnerProfileId")
      )

instance Prelude.Hashable ListedAgreement where
  hashWithSalt _salt ListedAgreement' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` localProfileId
      `Prelude.hashWithSalt` agreementId
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` partnerProfileId

instance Prelude.NFData ListedAgreement where
  rnf ListedAgreement' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf localProfileId
      `Prelude.seq` Prelude.rnf agreementId
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf partnerProfileId
