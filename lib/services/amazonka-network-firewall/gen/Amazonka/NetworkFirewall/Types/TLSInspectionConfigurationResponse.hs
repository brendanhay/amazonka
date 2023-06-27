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
-- Module      : Amazonka.NetworkFirewall.Types.TLSInspectionConfigurationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.TLSInspectionConfigurationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.EncryptionConfiguration
import Amazonka.NetworkFirewall.Types.ResourceStatus
import Amazonka.NetworkFirewall.Types.Tag
import Amazonka.NetworkFirewall.Types.TlsCertificateData
import qualified Amazonka.Prelude as Prelude

-- | The high-level properties of a TLS inspection configuration. This, along
-- with the @TLSInspectionConfiguration@, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling @DescribeTLSInspectionConfiguration@.
--
-- /See:/ 'newTLSInspectionConfigurationResponse' smart constructor.
data TLSInspectionConfigurationResponse = TLSInspectionConfigurationResponse'
  { -- | A list of the certificates associated with the TLS inspection
    -- configuration.
    certificates :: Prelude.Maybe [TlsCertificateData],
    -- | A description of the TLS inspection configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains the Amazon Web Services KMS encryption
    -- configuration settings for your TLS inspection configuration.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The last time that the TLS inspection configuration was changed.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The number of firewall policies that use this TLS inspection
    -- configuration.
    numberOfAssociations :: Prelude.Maybe Prelude.Int,
    -- | Detailed information about the current status of a
    -- TLSInspectionConfiguration. You can retrieve this for a TLS inspection
    -- configuration by calling DescribeTLSInspectionConfiguration and
    -- providing the TLS inspection configuration name and ARN.
    tLSInspectionConfigurationStatus :: Prelude.Maybe ResourceStatus,
    -- | The key:value pairs to associate with the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
    tLSInspectionConfigurationArn :: Prelude.Text,
    -- | The descriptive name of the TLS inspection configuration. You can\'t
    -- change the name of a TLS inspection configuration after you create it.
    tLSInspectionConfigurationName :: Prelude.Text,
    -- | A unique identifier for the TLS inspection configuration. This ID is
    -- returned in the responses to create and list commands. You provide it to
    -- operations such as update and delete.
    tLSInspectionConfigurationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TLSInspectionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificates', 'tLSInspectionConfigurationResponse_certificates' - A list of the certificates associated with the TLS inspection
-- configuration.
--
-- 'description', 'tLSInspectionConfigurationResponse_description' - A description of the TLS inspection configuration.
--
-- 'encryptionConfiguration', 'tLSInspectionConfigurationResponse_encryptionConfiguration' - A complex type that contains the Amazon Web Services KMS encryption
-- configuration settings for your TLS inspection configuration.
--
-- 'lastModifiedTime', 'tLSInspectionConfigurationResponse_lastModifiedTime' - The last time that the TLS inspection configuration was changed.
--
-- 'numberOfAssociations', 'tLSInspectionConfigurationResponse_numberOfAssociations' - The number of firewall policies that use this TLS inspection
-- configuration.
--
-- 'tLSInspectionConfigurationStatus', 'tLSInspectionConfigurationResponse_tLSInspectionConfigurationStatus' - Detailed information about the current status of a
-- TLSInspectionConfiguration. You can retrieve this for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration and
-- providing the TLS inspection configuration name and ARN.
--
-- 'tags', 'tLSInspectionConfigurationResponse_tags' - The key:value pairs to associate with the resource.
--
-- 'tLSInspectionConfigurationArn', 'tLSInspectionConfigurationResponse_tLSInspectionConfigurationArn' - The Amazon Resource Name (ARN) of the TLS inspection configuration.
--
-- 'tLSInspectionConfigurationName', 'tLSInspectionConfigurationResponse_tLSInspectionConfigurationName' - The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
--
-- 'tLSInspectionConfigurationId', 'tLSInspectionConfigurationResponse_tLSInspectionConfigurationId' - A unique identifier for the TLS inspection configuration. This ID is
-- returned in the responses to create and list commands. You provide it to
-- operations such as update and delete.
newTLSInspectionConfigurationResponse ::
  -- | 'tLSInspectionConfigurationArn'
  Prelude.Text ->
  -- | 'tLSInspectionConfigurationName'
  Prelude.Text ->
  -- | 'tLSInspectionConfigurationId'
  Prelude.Text ->
  TLSInspectionConfigurationResponse
newTLSInspectionConfigurationResponse
  pTLSInspectionConfigurationArn_
  pTLSInspectionConfigurationName_
  pTLSInspectionConfigurationId_ =
    TLSInspectionConfigurationResponse'
      { certificates =
          Prelude.Nothing,
        description = Prelude.Nothing,
        encryptionConfiguration =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        numberOfAssociations = Prelude.Nothing,
        tLSInspectionConfigurationStatus =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        tLSInspectionConfigurationArn =
          pTLSInspectionConfigurationArn_,
        tLSInspectionConfigurationName =
          pTLSInspectionConfigurationName_,
        tLSInspectionConfigurationId =
          pTLSInspectionConfigurationId_
      }

-- | A list of the certificates associated with the TLS inspection
-- configuration.
tLSInspectionConfigurationResponse_certificates :: Lens.Lens' TLSInspectionConfigurationResponse (Prelude.Maybe [TlsCertificateData])
tLSInspectionConfigurationResponse_certificates = Lens.lens (\TLSInspectionConfigurationResponse' {certificates} -> certificates) (\s@TLSInspectionConfigurationResponse' {} a -> s {certificates = a} :: TLSInspectionConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | A description of the TLS inspection configuration.
tLSInspectionConfigurationResponse_description :: Lens.Lens' TLSInspectionConfigurationResponse (Prelude.Maybe Prelude.Text)
tLSInspectionConfigurationResponse_description = Lens.lens (\TLSInspectionConfigurationResponse' {description} -> description) (\s@TLSInspectionConfigurationResponse' {} a -> s {description = a} :: TLSInspectionConfigurationResponse)

-- | A complex type that contains the Amazon Web Services KMS encryption
-- configuration settings for your TLS inspection configuration.
tLSInspectionConfigurationResponse_encryptionConfiguration :: Lens.Lens' TLSInspectionConfigurationResponse (Prelude.Maybe EncryptionConfiguration)
tLSInspectionConfigurationResponse_encryptionConfiguration = Lens.lens (\TLSInspectionConfigurationResponse' {encryptionConfiguration} -> encryptionConfiguration) (\s@TLSInspectionConfigurationResponse' {} a -> s {encryptionConfiguration = a} :: TLSInspectionConfigurationResponse)

-- | The last time that the TLS inspection configuration was changed.
tLSInspectionConfigurationResponse_lastModifiedTime :: Lens.Lens' TLSInspectionConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
tLSInspectionConfigurationResponse_lastModifiedTime = Lens.lens (\TLSInspectionConfigurationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@TLSInspectionConfigurationResponse' {} a -> s {lastModifiedTime = a} :: TLSInspectionConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | The number of firewall policies that use this TLS inspection
-- configuration.
tLSInspectionConfigurationResponse_numberOfAssociations :: Lens.Lens' TLSInspectionConfigurationResponse (Prelude.Maybe Prelude.Int)
tLSInspectionConfigurationResponse_numberOfAssociations = Lens.lens (\TLSInspectionConfigurationResponse' {numberOfAssociations} -> numberOfAssociations) (\s@TLSInspectionConfigurationResponse' {} a -> s {numberOfAssociations = a} :: TLSInspectionConfigurationResponse)

-- | Detailed information about the current status of a
-- TLSInspectionConfiguration. You can retrieve this for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration and
-- providing the TLS inspection configuration name and ARN.
tLSInspectionConfigurationResponse_tLSInspectionConfigurationStatus :: Lens.Lens' TLSInspectionConfigurationResponse (Prelude.Maybe ResourceStatus)
tLSInspectionConfigurationResponse_tLSInspectionConfigurationStatus = Lens.lens (\TLSInspectionConfigurationResponse' {tLSInspectionConfigurationStatus} -> tLSInspectionConfigurationStatus) (\s@TLSInspectionConfigurationResponse' {} a -> s {tLSInspectionConfigurationStatus = a} :: TLSInspectionConfigurationResponse)

-- | The key:value pairs to associate with the resource.
tLSInspectionConfigurationResponse_tags :: Lens.Lens' TLSInspectionConfigurationResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
tLSInspectionConfigurationResponse_tags = Lens.lens (\TLSInspectionConfigurationResponse' {tags} -> tags) (\s@TLSInspectionConfigurationResponse' {} a -> s {tags = a} :: TLSInspectionConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
tLSInspectionConfigurationResponse_tLSInspectionConfigurationArn :: Lens.Lens' TLSInspectionConfigurationResponse Prelude.Text
tLSInspectionConfigurationResponse_tLSInspectionConfigurationArn = Lens.lens (\TLSInspectionConfigurationResponse' {tLSInspectionConfigurationArn} -> tLSInspectionConfigurationArn) (\s@TLSInspectionConfigurationResponse' {} a -> s {tLSInspectionConfigurationArn = a} :: TLSInspectionConfigurationResponse)

-- | The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
tLSInspectionConfigurationResponse_tLSInspectionConfigurationName :: Lens.Lens' TLSInspectionConfigurationResponse Prelude.Text
tLSInspectionConfigurationResponse_tLSInspectionConfigurationName = Lens.lens (\TLSInspectionConfigurationResponse' {tLSInspectionConfigurationName} -> tLSInspectionConfigurationName) (\s@TLSInspectionConfigurationResponse' {} a -> s {tLSInspectionConfigurationName = a} :: TLSInspectionConfigurationResponse)

-- | A unique identifier for the TLS inspection configuration. This ID is
-- returned in the responses to create and list commands. You provide it to
-- operations such as update and delete.
tLSInspectionConfigurationResponse_tLSInspectionConfigurationId :: Lens.Lens' TLSInspectionConfigurationResponse Prelude.Text
tLSInspectionConfigurationResponse_tLSInspectionConfigurationId = Lens.lens (\TLSInspectionConfigurationResponse' {tLSInspectionConfigurationId} -> tLSInspectionConfigurationId) (\s@TLSInspectionConfigurationResponse' {} a -> s {tLSInspectionConfigurationId = a} :: TLSInspectionConfigurationResponse)

instance
  Data.FromJSON
    TLSInspectionConfigurationResponse
  where
  parseJSON =
    Data.withObject
      "TLSInspectionConfigurationResponse"
      ( \x ->
          TLSInspectionConfigurationResponse'
            Prelude.<$> (x Data..:? "Certificates" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EncryptionConfiguration")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "NumberOfAssociations")
            Prelude.<*> (x Data..:? "TLSInspectionConfigurationStatus")
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..: "TLSInspectionConfigurationArn")
            Prelude.<*> (x Data..: "TLSInspectionConfigurationName")
            Prelude.<*> (x Data..: "TLSInspectionConfigurationId")
      )

instance
  Prelude.Hashable
    TLSInspectionConfigurationResponse
  where
  hashWithSalt
    _salt
    TLSInspectionConfigurationResponse' {..} =
      _salt
        `Prelude.hashWithSalt` certificates
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` encryptionConfiguration
        `Prelude.hashWithSalt` lastModifiedTime
        `Prelude.hashWithSalt` numberOfAssociations
        `Prelude.hashWithSalt` tLSInspectionConfigurationStatus
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` tLSInspectionConfigurationArn
        `Prelude.hashWithSalt` tLSInspectionConfigurationName
        `Prelude.hashWithSalt` tLSInspectionConfigurationId

instance
  Prelude.NFData
    TLSInspectionConfigurationResponse
  where
  rnf TLSInspectionConfigurationResponse' {..} =
    Prelude.rnf certificates
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf numberOfAssociations
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationArn
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationName
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurationId
