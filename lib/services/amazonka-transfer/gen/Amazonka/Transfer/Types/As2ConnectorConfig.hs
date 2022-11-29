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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.As2ConnectorConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | The algorithm that is used to encrypt the file.
    --
    -- You can only specify @NONE@ if the URL for your connector uses HTTPS.
    -- This ensures that no traffic is sent in clear text.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlg,
    -- | Specifies whether the AS2 file is compressed.
    compression :: Prelude.Maybe CompressionEnum,
    -- | The signing algorithm for the MDN response.
    --
    -- If set to DEFAULT (or not set at all), the value for @SigningAlgorithm@
    -- is used.
    mdnSigningAlgorithm :: Prelude.Maybe MdnSigningAlg,
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
    -- | Used as the @Subject@ HTTP header attribute in AS2 messages that are
    -- being sent with the connector.
    messageSubject :: Prelude.Maybe Prelude.Text,
    -- | The algorithm that is used to sign the AS2 messages sent with the
    -- connector.
    signingAlgorithm :: Prelude.Maybe SigningAlg,
    -- | A unique identifier for the partner profile for the connector.
    partnerProfileId :: Prelude.Maybe Prelude.Text
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
-- 'encryptionAlgorithm', 'as2ConnectorConfig_encryptionAlgorithm' - The algorithm that is used to encrypt the file.
--
-- You can only specify @NONE@ if the URL for your connector uses HTTPS.
-- This ensures that no traffic is sent in clear text.
--
-- 'compression', 'as2ConnectorConfig_compression' - Specifies whether the AS2 file is compressed.
--
-- 'mdnSigningAlgorithm', 'as2ConnectorConfig_mdnSigningAlgorithm' - The signing algorithm for the MDN response.
--
-- If set to DEFAULT (or not set at all), the value for @SigningAlgorithm@
-- is used.
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
-- 'messageSubject', 'as2ConnectorConfig_messageSubject' - Used as the @Subject@ HTTP header attribute in AS2 messages that are
-- being sent with the connector.
--
-- 'signingAlgorithm', 'as2ConnectorConfig_signingAlgorithm' - The algorithm that is used to sign the AS2 messages sent with the
-- connector.
--
-- 'partnerProfileId', 'as2ConnectorConfig_partnerProfileId' - A unique identifier for the partner profile for the connector.
newAs2ConnectorConfig ::
  As2ConnectorConfig
newAs2ConnectorConfig =
  As2ConnectorConfig'
    { encryptionAlgorithm =
        Prelude.Nothing,
      compression = Prelude.Nothing,
      mdnSigningAlgorithm = Prelude.Nothing,
      localProfileId = Prelude.Nothing,
      mdnResponse = Prelude.Nothing,
      messageSubject = Prelude.Nothing,
      signingAlgorithm = Prelude.Nothing,
      partnerProfileId = Prelude.Nothing
    }

-- | The algorithm that is used to encrypt the file.
--
-- You can only specify @NONE@ if the URL for your connector uses HTTPS.
-- This ensures that no traffic is sent in clear text.
as2ConnectorConfig_encryptionAlgorithm :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe EncryptionAlg)
as2ConnectorConfig_encryptionAlgorithm = Lens.lens (\As2ConnectorConfig' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@As2ConnectorConfig' {} a -> s {encryptionAlgorithm = a} :: As2ConnectorConfig)

-- | Specifies whether the AS2 file is compressed.
as2ConnectorConfig_compression :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe CompressionEnum)
as2ConnectorConfig_compression = Lens.lens (\As2ConnectorConfig' {compression} -> compression) (\s@As2ConnectorConfig' {} a -> s {compression = a} :: As2ConnectorConfig)

-- | The signing algorithm for the MDN response.
--
-- If set to DEFAULT (or not set at all), the value for @SigningAlgorithm@
-- is used.
as2ConnectorConfig_mdnSigningAlgorithm :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe MdnSigningAlg)
as2ConnectorConfig_mdnSigningAlgorithm = Lens.lens (\As2ConnectorConfig' {mdnSigningAlgorithm} -> mdnSigningAlgorithm) (\s@As2ConnectorConfig' {} a -> s {mdnSigningAlgorithm = a} :: As2ConnectorConfig)

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

-- | Used as the @Subject@ HTTP header attribute in AS2 messages that are
-- being sent with the connector.
as2ConnectorConfig_messageSubject :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe Prelude.Text)
as2ConnectorConfig_messageSubject = Lens.lens (\As2ConnectorConfig' {messageSubject} -> messageSubject) (\s@As2ConnectorConfig' {} a -> s {messageSubject = a} :: As2ConnectorConfig)

-- | The algorithm that is used to sign the AS2 messages sent with the
-- connector.
as2ConnectorConfig_signingAlgorithm :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe SigningAlg)
as2ConnectorConfig_signingAlgorithm = Lens.lens (\As2ConnectorConfig' {signingAlgorithm} -> signingAlgorithm) (\s@As2ConnectorConfig' {} a -> s {signingAlgorithm = a} :: As2ConnectorConfig)

-- | A unique identifier for the partner profile for the connector.
as2ConnectorConfig_partnerProfileId :: Lens.Lens' As2ConnectorConfig (Prelude.Maybe Prelude.Text)
as2ConnectorConfig_partnerProfileId = Lens.lens (\As2ConnectorConfig' {partnerProfileId} -> partnerProfileId) (\s@As2ConnectorConfig' {} a -> s {partnerProfileId = a} :: As2ConnectorConfig)

instance Core.FromJSON As2ConnectorConfig where
  parseJSON =
    Core.withObject
      "As2ConnectorConfig"
      ( \x ->
          As2ConnectorConfig'
            Prelude.<$> (x Core..:? "EncryptionAlgorithm")
            Prelude.<*> (x Core..:? "Compression")
            Prelude.<*> (x Core..:? "MdnSigningAlgorithm")
            Prelude.<*> (x Core..:? "LocalProfileId")
            Prelude.<*> (x Core..:? "MdnResponse")
            Prelude.<*> (x Core..:? "MessageSubject")
            Prelude.<*> (x Core..:? "SigningAlgorithm")
            Prelude.<*> (x Core..:? "PartnerProfileId")
      )

instance Prelude.Hashable As2ConnectorConfig where
  hashWithSalt _salt As2ConnectorConfig' {..} =
    _salt `Prelude.hashWithSalt` encryptionAlgorithm
      `Prelude.hashWithSalt` compression
      `Prelude.hashWithSalt` mdnSigningAlgorithm
      `Prelude.hashWithSalt` localProfileId
      `Prelude.hashWithSalt` mdnResponse
      `Prelude.hashWithSalt` messageSubject
      `Prelude.hashWithSalt` signingAlgorithm
      `Prelude.hashWithSalt` partnerProfileId

instance Prelude.NFData As2ConnectorConfig where
  rnf As2ConnectorConfig' {..} =
    Prelude.rnf encryptionAlgorithm
      `Prelude.seq` Prelude.rnf compression
      `Prelude.seq` Prelude.rnf mdnSigningAlgorithm
      `Prelude.seq` Prelude.rnf localProfileId
      `Prelude.seq` Prelude.rnf mdnResponse
      `Prelude.seq` Prelude.rnf messageSubject
      `Prelude.seq` Prelude.rnf signingAlgorithm
      `Prelude.seq` Prelude.rnf partnerProfileId

instance Core.ToJSON As2ConnectorConfig where
  toJSON As2ConnectorConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EncryptionAlgorithm" Core..=)
              Prelude.<$> encryptionAlgorithm,
            ("Compression" Core..=) Prelude.<$> compression,
            ("MdnSigningAlgorithm" Core..=)
              Prelude.<$> mdnSigningAlgorithm,
            ("LocalProfileId" Core..=)
              Prelude.<$> localProfileId,
            ("MdnResponse" Core..=) Prelude.<$> mdnResponse,
            ("MessageSubject" Core..=)
              Prelude.<$> messageSubject,
            ("SigningAlgorithm" Core..=)
              Prelude.<$> signingAlgorithm,
            ("PartnerProfileId" Core..=)
              Prelude.<$> partnerProfileId
          ]
      )
