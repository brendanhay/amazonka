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
-- Module      : Amazonka.Transfer.Types.As2ConnectorConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.As2ConnectorConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.CompressionEnum
import Amazonka.Transfer.Types.EncryptionAlg
import Amazonka.Transfer.Types.MdnResponse
import Amazonka.Transfer.Types.MdnSigningAlg
import Amazonka.Transfer.Types.SigningAlg

-- | Contains the details for a connector object. The connector object is
-- used for AS2 outbound processes, to connect the Transfer Family customer
-- with the trading partner.
--
-- /See:/ 'newAs2ConnectorConfig' smart constructor.
data As2ConnectorConfig = As2ConnectorConfig'
  { -- | Specifies whether the AS2 file is compressed.
    compression :: Prelude.Maybe CompressionEnum,
    -- | The algorithm that is used to encrypt the file.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlg,
    -- | A unique identifier for the AS2 local profile.
    localProfileId :: Prelude.Maybe Prelude.Text,
    -- | Used for outbound requests (from an Transfer Family server to a partner
    -- AS2 server) to determine whether the partner response for transfers is
    -- synchronous or asynchronous. Specify either of the following values:
    --
    -- -   @SYNC@: The system expects a synchronous MDN response, confirming
    --     that the file was transferred successfully (or not).
    --
    -- -   @NONE@: Specifies that no MDN response is required.
    mdnResponse :: Prelude.Maybe MdnResponse,
    -- | The signing algorithm for the MDN response.
    --
    -- If set to DEFAULT (or not set at all), the value for @SigningAlogorithm@
    -- is used.
    mdnSigningAlgorithm :: Prelude.Maybe MdnSigningAlg,
    -- | Used as the @Subject@ HTTP header attribute in AS2 messages that are
    -- being sent with the connector.
    messageSubject :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the partner profile for the connector.
    partnerProfileId :: Prelude.Maybe Prelude.Text,
    -- | The algorithm that is used to sign the AS2 messages sent with the
    -- connector.
    signingAlgorithm :: Prelude.Maybe SigningAlg
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'As2ConnectorConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compression', 'as2ConnectorConfig_compression' - Specifies whether the AS2 file is compressed.
--
-- 'encryptionAlgorithm', 'as2ConnectorConfig_encryptionAlgorithm' - The algorithm that is used to encrypt the file.
--
-- 'localProfileId', 'as2ConnectorConfig_localProfileId' - A unique identifier for the AS2 local profile.
--
-- 'mdnResponse', 'as2ConnectorConfig_mdnResponse' - Used for outbound requests (from an Transfer Family server to a partner
-- AS2 server) to determine whether the partner response for transfers is
-- synchronous or asynchronous. Specify either of the following values:
--
-- -   @SYNC@: The system expects a synchronous MDN response, confirming
--     that the file was transferred successfully (or not).
--
-- -   @NONE@: Specifies that no MDN response is required.
--
-- 'mdnSigningAlgorithm', 'as2ConnectorConfig_mdnSigningAlgorithm' - The signing algorithm for the MDN response.
--
-- If set to DEFAULT (or not set at all), the value for @SigningAlogorithm@
-- is used.
--
-- 'messageSubject', 'as2ConnectorConfig_messageSubject' - Used as the @Subject@ HTTP header attribute in AS2 messages that are
-- being sent with the connector.
--
-- 'partnerProfileId', 'as2ConnectorConfig_partnerProfileId' - A unique identifier for the partner profile for the connector.
--
-- 'signingAlgorithm', 'as2ConnectorConfig_signingAlgorithm' - The algorithm that is used to sign the AS2 messages sent with the
-- connector.
newAs2ConnectorConfig ::
  As2ConnectorConfig
newAs2ConnectorConfig =
  As2ConnectorConfig'
    { compression = Prelude.Nothing,
      encryptionAlgorithm = Prelude.Nothing,
      localProfileId = Prelude.Nothing,
      mdnResponse = Prelude.Nothing,
      mdnSigningAlgorithm = Prelude.Nothing,
      messageSubject = Prelude.Nothing,
      partnerProfileId = Prelude.Nothing,
      signingAlgorithm = Prelude.Nothing
    }

-- | Specifies whether the AS2 file is compressed.
as2ConnectorConfig_compression :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe CompressionEnum)
as2ConnectorConfig_compression = Lens.lens (\As2ConnectorConfig' {compression} -> compression) (\s@As2ConnectorConfig' {} a -> s {compression = a} :: As2ConnectorConfig)

-- | The algorithm that is used to encrypt the file.
as2ConnectorConfig_encryptionAlgorithm :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe EncryptionAlg)
as2ConnectorConfig_encryptionAlgorithm = Lens.lens (\As2ConnectorConfig' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@As2ConnectorConfig' {} a -> s {encryptionAlgorithm = a} :: As2ConnectorConfig)

-- | A unique identifier for the AS2 local profile.
as2ConnectorConfig_localProfileId :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe Prelude.Text)
as2ConnectorConfig_localProfileId = Lens.lens (\As2ConnectorConfig' {localProfileId} -> localProfileId) (\s@As2ConnectorConfig' {} a -> s {localProfileId = a} :: As2ConnectorConfig)

-- | Used for outbound requests (from an Transfer Family server to a partner
-- AS2 server) to determine whether the partner response for transfers is
-- synchronous or asynchronous. Specify either of the following values:
--
-- -   @SYNC@: The system expects a synchronous MDN response, confirming
--     that the file was transferred successfully (or not).
--
-- -   @NONE@: Specifies that no MDN response is required.
as2ConnectorConfig_mdnResponse :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe MdnResponse)
as2ConnectorConfig_mdnResponse = Lens.lens (\As2ConnectorConfig' {mdnResponse} -> mdnResponse) (\s@As2ConnectorConfig' {} a -> s {mdnResponse = a} :: As2ConnectorConfig)

-- | The signing algorithm for the MDN response.
--
-- If set to DEFAULT (or not set at all), the value for @SigningAlogorithm@
-- is used.
as2ConnectorConfig_mdnSigningAlgorithm :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe MdnSigningAlg)
as2ConnectorConfig_mdnSigningAlgorithm = Lens.lens (\As2ConnectorConfig' {mdnSigningAlgorithm} -> mdnSigningAlgorithm) (\s@As2ConnectorConfig' {} a -> s {mdnSigningAlgorithm = a} :: As2ConnectorConfig)

-- | Used as the @Subject@ HTTP header attribute in AS2 messages that are
-- being sent with the connector.
as2ConnectorConfig_messageSubject :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe Prelude.Text)
as2ConnectorConfig_messageSubject = Lens.lens (\As2ConnectorConfig' {messageSubject} -> messageSubject) (\s@As2ConnectorConfig' {} a -> s {messageSubject = a} :: As2ConnectorConfig)

-- | A unique identifier for the partner profile for the connector.
as2ConnectorConfig_partnerProfileId :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe Prelude.Text)
as2ConnectorConfig_partnerProfileId = Lens.lens (\As2ConnectorConfig' {partnerProfileId} -> partnerProfileId) (\s@As2ConnectorConfig' {} a -> s {partnerProfileId = a} :: As2ConnectorConfig)

-- | The algorithm that is used to sign the AS2 messages sent with the
-- connector.
as2ConnectorConfig_signingAlgorithm :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe SigningAlg)
as2ConnectorConfig_signingAlgorithm = Lens.lens (\As2ConnectorConfig' {signingAlgorithm} -> signingAlgorithm) (\s@As2ConnectorConfig' {} a -> s {signingAlgorithm = a} :: As2ConnectorConfig)

instance Data.FromJSON As2ConnectorConfig where
  parseJSON =
    Data.withObject
      "As2ConnectorConfig"
      ( \x ->
          As2ConnectorConfig'
            Prelude.<$> (x Data..:? "Compression")
            Prelude.<*> (x Data..:? "EncryptionAlgorithm")
            Prelude.<*> (x Data..:? "LocalProfileId")
            Prelude.<*> (x Data..:? "MdnResponse")
            Prelude.<*> (x Data..:? "MdnSigningAlgorithm")
            Prelude.<*> (x Data..:? "MessageSubject")
            Prelude.<*> (x Data..:? "PartnerProfileId")
            Prelude.<*> (x Data..:? "SigningAlgorithm")
      )

instance Prelude.Hashable As2ConnectorConfig where
  hashWithSalt _salt As2ConnectorConfig' {..} =
    _salt
      `Prelude.hashWithSalt` compression
      `Prelude.hashWithSalt` encryptionAlgorithm
      `Prelude.hashWithSalt` localProfileId
      `Prelude.hashWithSalt` mdnResponse
      `Prelude.hashWithSalt` mdnSigningAlgorithm
      `Prelude.hashWithSalt` messageSubject
      `Prelude.hashWithSalt` partnerProfileId
      `Prelude.hashWithSalt` signingAlgorithm

instance Prelude.NFData As2ConnectorConfig where
  rnf As2ConnectorConfig' {..} =
    Prelude.rnf compression
      `Prelude.seq` Prelude.rnf encryptionAlgorithm
      `Prelude.seq` Prelude.rnf localProfileId
      `Prelude.seq` Prelude.rnf mdnResponse
      `Prelude.seq` Prelude.rnf mdnSigningAlgorithm
      `Prelude.seq` Prelude.rnf messageSubject
      `Prelude.seq` Prelude.rnf partnerProfileId
      `Prelude.seq` Prelude.rnf signingAlgorithm

instance Data.ToJSON As2ConnectorConfig where
  toJSON As2ConnectorConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Compression" Data..=) Prelude.<$> compression,
            ("EncryptionAlgorithm" Data..=)
              Prelude.<$> encryptionAlgorithm,
            ("LocalProfileId" Data..=)
              Prelude.<$> localProfileId,
            ("MdnResponse" Data..=) Prelude.<$> mdnResponse,
            ("MdnSigningAlgorithm" Data..=)
              Prelude.<$> mdnSigningAlgorithm,
            ("MessageSubject" Data..=)
              Prelude.<$> messageSubject,
            ("PartnerProfileId" Data..=)
              Prelude.<$> partnerProfileId,
            ("SigningAlgorithm" Data..=)
              Prelude.<$> signingAlgorithm
          ]
      )
