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
-- Module      : Amazonka.Transfer.Types.DescribedAgreement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedAgreement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.AgreementStatusType
import Amazonka.Transfer.Types.Tag

-- | Describes the properties of an agreement.
--
-- /See:/ 'newDescribedAgreement' smart constructor.
data DescribedAgreement = DescribedAgreement'
  { -- | Key-value pairs that can be used to group and search for agreements.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | With AS2, you can send files by calling @StartFileTransfer@ and
    -- specifying the file paths in the request parameter, @SendFilePaths@. We
    -- use the file’s parent directory (for example, for
    -- @--send-file-paths \/bucket\/dir\/file.txt@, parent directory is
    -- @\/bucket\/dir\/@) to temporarily store a processed AS2 message file,
    -- store the MDN when we receive them from the partner, and write a final
    -- JSON file containing relevant metadata of the transmission. So, the
    -- @AccessRole@ needs to provide read and write access to the parent
    -- directory of the file location used in the @StartFileTransfer@ request.
    -- Additionally, you need to provide read and write access to the parent
    -- directory of the files that you intend to send with @StartFileTransfer@.
    accessRole :: Prelude.Maybe Prelude.Text,
    -- | The current status of the agreement, either @ACTIVE@ or @INACTIVE@.
    status :: Prelude.Maybe AgreementStatusType,
    -- | The landing directory (folder) for files that are transferred by using
    -- the AS2 protocol.
    baseDirectory :: Prelude.Maybe Prelude.Text,
    -- | The name or short description that\'s used to identify the agreement.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the AS2 local profile.
    localProfileId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the agreement. This identifier is returned when
    -- you create an agreement.
    agreementId :: Prelude.Maybe Prelude.Text,
    -- | A system-assigned unique identifier for a server instance. This
    -- identifier indicates the specific server that the agreement uses.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the partner profile used in the agreement.
    partnerProfileId :: Prelude.Maybe Prelude.Text,
    -- | The unique Amazon Resource Name (ARN) for the agreement.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribedAgreement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describedAgreement_tags' - Key-value pairs that can be used to group and search for agreements.
--
-- 'accessRole', 'describedAgreement_accessRole' - With AS2, you can send files by calling @StartFileTransfer@ and
-- specifying the file paths in the request parameter, @SendFilePaths@. We
-- use the file’s parent directory (for example, for
-- @--send-file-paths \/bucket\/dir\/file.txt@, parent directory is
-- @\/bucket\/dir\/@) to temporarily store a processed AS2 message file,
-- store the MDN when we receive them from the partner, and write a final
-- JSON file containing relevant metadata of the transmission. So, the
-- @AccessRole@ needs to provide read and write access to the parent
-- directory of the file location used in the @StartFileTransfer@ request.
-- Additionally, you need to provide read and write access to the parent
-- directory of the files that you intend to send with @StartFileTransfer@.
--
-- 'status', 'describedAgreement_status' - The current status of the agreement, either @ACTIVE@ or @INACTIVE@.
--
-- 'baseDirectory', 'describedAgreement_baseDirectory' - The landing directory (folder) for files that are transferred by using
-- the AS2 protocol.
--
-- 'description', 'describedAgreement_description' - The name or short description that\'s used to identify the agreement.
--
-- 'localProfileId', 'describedAgreement_localProfileId' - A unique identifier for the AS2 local profile.
--
-- 'agreementId', 'describedAgreement_agreementId' - A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
--
-- 'serverId', 'describedAgreement_serverId' - A system-assigned unique identifier for a server instance. This
-- identifier indicates the specific server that the agreement uses.
--
-- 'partnerProfileId', 'describedAgreement_partnerProfileId' - A unique identifier for the partner profile used in the agreement.
--
-- 'arn', 'describedAgreement_arn' - The unique Amazon Resource Name (ARN) for the agreement.
newDescribedAgreement ::
  -- | 'arn'
  Prelude.Text ->
  DescribedAgreement
newDescribedAgreement pArn_ =
  DescribedAgreement'
    { tags = Prelude.Nothing,
      accessRole = Prelude.Nothing,
      status = Prelude.Nothing,
      baseDirectory = Prelude.Nothing,
      description = Prelude.Nothing,
      localProfileId = Prelude.Nothing,
      agreementId = Prelude.Nothing,
      serverId = Prelude.Nothing,
      partnerProfileId = Prelude.Nothing,
      arn = pArn_
    }

-- | Key-value pairs that can be used to group and search for agreements.
describedAgreement_tags :: Lens.Lens' DescribedAgreement (Prelude.Maybe (Prelude.NonEmpty Tag))
describedAgreement_tags = Lens.lens (\DescribedAgreement' {tags} -> tags) (\s@DescribedAgreement' {} a -> s {tags = a} :: DescribedAgreement) Prelude.. Lens.mapping Lens.coerced

-- | With AS2, you can send files by calling @StartFileTransfer@ and
-- specifying the file paths in the request parameter, @SendFilePaths@. We
-- use the file’s parent directory (for example, for
-- @--send-file-paths \/bucket\/dir\/file.txt@, parent directory is
-- @\/bucket\/dir\/@) to temporarily store a processed AS2 message file,
-- store the MDN when we receive them from the partner, and write a final
-- JSON file containing relevant metadata of the transmission. So, the
-- @AccessRole@ needs to provide read and write access to the parent
-- directory of the file location used in the @StartFileTransfer@ request.
-- Additionally, you need to provide read and write access to the parent
-- directory of the files that you intend to send with @StartFileTransfer@.
describedAgreement_accessRole :: Lens.Lens' DescribedAgreement (Prelude.Maybe Prelude.Text)
describedAgreement_accessRole = Lens.lens (\DescribedAgreement' {accessRole} -> accessRole) (\s@DescribedAgreement' {} a -> s {accessRole = a} :: DescribedAgreement)

-- | The current status of the agreement, either @ACTIVE@ or @INACTIVE@.
describedAgreement_status :: Lens.Lens' DescribedAgreement (Prelude.Maybe AgreementStatusType)
describedAgreement_status = Lens.lens (\DescribedAgreement' {status} -> status) (\s@DescribedAgreement' {} a -> s {status = a} :: DescribedAgreement)

-- | The landing directory (folder) for files that are transferred by using
-- the AS2 protocol.
describedAgreement_baseDirectory :: Lens.Lens' DescribedAgreement (Prelude.Maybe Prelude.Text)
describedAgreement_baseDirectory = Lens.lens (\DescribedAgreement' {baseDirectory} -> baseDirectory) (\s@DescribedAgreement' {} a -> s {baseDirectory = a} :: DescribedAgreement)

-- | The name or short description that\'s used to identify the agreement.
describedAgreement_description :: Lens.Lens' DescribedAgreement (Prelude.Maybe Prelude.Text)
describedAgreement_description = Lens.lens (\DescribedAgreement' {description} -> description) (\s@DescribedAgreement' {} a -> s {description = a} :: DescribedAgreement)

-- | A unique identifier for the AS2 local profile.
describedAgreement_localProfileId :: Lens.Lens' DescribedAgreement (Prelude.Maybe Prelude.Text)
describedAgreement_localProfileId = Lens.lens (\DescribedAgreement' {localProfileId} -> localProfileId) (\s@DescribedAgreement' {} a -> s {localProfileId = a} :: DescribedAgreement)

-- | A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
describedAgreement_agreementId :: Lens.Lens' DescribedAgreement (Prelude.Maybe Prelude.Text)
describedAgreement_agreementId = Lens.lens (\DescribedAgreement' {agreementId} -> agreementId) (\s@DescribedAgreement' {} a -> s {agreementId = a} :: DescribedAgreement)

-- | A system-assigned unique identifier for a server instance. This
-- identifier indicates the specific server that the agreement uses.
describedAgreement_serverId :: Lens.Lens' DescribedAgreement (Prelude.Maybe Prelude.Text)
describedAgreement_serverId = Lens.lens (\DescribedAgreement' {serverId} -> serverId) (\s@DescribedAgreement' {} a -> s {serverId = a} :: DescribedAgreement)

-- | A unique identifier for the partner profile used in the agreement.
describedAgreement_partnerProfileId :: Lens.Lens' DescribedAgreement (Prelude.Maybe Prelude.Text)
describedAgreement_partnerProfileId = Lens.lens (\DescribedAgreement' {partnerProfileId} -> partnerProfileId) (\s@DescribedAgreement' {} a -> s {partnerProfileId = a} :: DescribedAgreement)

-- | The unique Amazon Resource Name (ARN) for the agreement.
describedAgreement_arn :: Lens.Lens' DescribedAgreement Prelude.Text
describedAgreement_arn = Lens.lens (\DescribedAgreement' {arn} -> arn) (\s@DescribedAgreement' {} a -> s {arn = a} :: DescribedAgreement)

instance Data.FromJSON DescribedAgreement where
  parseJSON =
    Data.withObject
      "DescribedAgreement"
      ( \x ->
          DescribedAgreement'
            Prelude.<$> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "AccessRole")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "BaseDirectory")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LocalProfileId")
            Prelude.<*> (x Data..:? "AgreementId")
            Prelude.<*> (x Data..:? "ServerId")
            Prelude.<*> (x Data..:? "PartnerProfileId")
            Prelude.<*> (x Data..: "Arn")
      )

instance Prelude.Hashable DescribedAgreement where
  hashWithSalt _salt DescribedAgreement' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` accessRole
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` baseDirectory
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` localProfileId
      `Prelude.hashWithSalt` agreementId
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` partnerProfileId
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribedAgreement where
  rnf DescribedAgreement' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf accessRole
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf baseDirectory
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf localProfileId
      `Prelude.seq` Prelude.rnf agreementId
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf partnerProfileId
      `Prelude.seq` Prelude.rnf arn
