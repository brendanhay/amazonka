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
-- Module      : Amazonka.Transfer.Types.DescribedConnector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedConnector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.As2ConnectorConfig
import Amazonka.Transfer.Types.Tag

-- | Describes the parameters for the connector, as identified by the
-- @ConnectorId@.
--
-- /See:/ 'newDescribedConnector' smart constructor.
data DescribedConnector = DescribedConnector'
  { -- | Key-value pairs that can be used to group and search for connectors.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The unique identifier for the connector.
    connectorId :: Prelude.Maybe Prelude.Text,
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
    -- | The URL of the partner\'s AS2 endpoint.
    url :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains the parameters for a connector object.
    as2Config :: Prelude.Maybe As2ConnectorConfig,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that allows a connector to turn on CloudWatch logging for
    -- Amazon S3 events. When set, you can view connector activity in your
    -- CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | The unique Amazon Resource Name (ARN) for the connector.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribedConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describedConnector_tags' - Key-value pairs that can be used to group and search for connectors.
--
-- 'connectorId', 'describedConnector_connectorId' - The unique identifier for the connector.
--
-- 'accessRole', 'describedConnector_accessRole' - With AS2, you can send files by calling @StartFileTransfer@ and
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
-- 'url', 'describedConnector_url' - The URL of the partner\'s AS2 endpoint.
--
-- 'as2Config', 'describedConnector_as2Config' - A structure that contains the parameters for a connector object.
--
-- 'loggingRole', 'describedConnector_loggingRole' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a connector to turn on CloudWatch logging for
-- Amazon S3 events. When set, you can view connector activity in your
-- CloudWatch logs.
--
-- 'arn', 'describedConnector_arn' - The unique Amazon Resource Name (ARN) for the connector.
newDescribedConnector ::
  -- | 'arn'
  Prelude.Text ->
  DescribedConnector
newDescribedConnector pArn_ =
  DescribedConnector'
    { tags = Prelude.Nothing,
      connectorId = Prelude.Nothing,
      accessRole = Prelude.Nothing,
      url = Prelude.Nothing,
      as2Config = Prelude.Nothing,
      loggingRole = Prelude.Nothing,
      arn = pArn_
    }

-- | Key-value pairs that can be used to group and search for connectors.
describedConnector_tags :: Lens.Lens' DescribedConnector (Prelude.Maybe (Prelude.NonEmpty Tag))
describedConnector_tags = Lens.lens (\DescribedConnector' {tags} -> tags) (\s@DescribedConnector' {} a -> s {tags = a} :: DescribedConnector) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the connector.
describedConnector_connectorId :: Lens.Lens' DescribedConnector (Prelude.Maybe Prelude.Text)
describedConnector_connectorId = Lens.lens (\DescribedConnector' {connectorId} -> connectorId) (\s@DescribedConnector' {} a -> s {connectorId = a} :: DescribedConnector)

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
describedConnector_accessRole :: Lens.Lens' DescribedConnector (Prelude.Maybe Prelude.Text)
describedConnector_accessRole = Lens.lens (\DescribedConnector' {accessRole} -> accessRole) (\s@DescribedConnector' {} a -> s {accessRole = a} :: DescribedConnector)

-- | The URL of the partner\'s AS2 endpoint.
describedConnector_url :: Lens.Lens' DescribedConnector (Prelude.Maybe Prelude.Text)
describedConnector_url = Lens.lens (\DescribedConnector' {url} -> url) (\s@DescribedConnector' {} a -> s {url = a} :: DescribedConnector)

-- | A structure that contains the parameters for a connector object.
describedConnector_as2Config :: Lens.Lens' DescribedConnector (Prelude.Maybe As2ConnectorConfig)
describedConnector_as2Config = Lens.lens (\DescribedConnector' {as2Config} -> as2Config) (\s@DescribedConnector' {} a -> s {as2Config = a} :: DescribedConnector)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a connector to turn on CloudWatch logging for
-- Amazon S3 events. When set, you can view connector activity in your
-- CloudWatch logs.
describedConnector_loggingRole :: Lens.Lens' DescribedConnector (Prelude.Maybe Prelude.Text)
describedConnector_loggingRole = Lens.lens (\DescribedConnector' {loggingRole} -> loggingRole) (\s@DescribedConnector' {} a -> s {loggingRole = a} :: DescribedConnector)

-- | The unique Amazon Resource Name (ARN) for the connector.
describedConnector_arn :: Lens.Lens' DescribedConnector Prelude.Text
describedConnector_arn = Lens.lens (\DescribedConnector' {arn} -> arn) (\s@DescribedConnector' {} a -> s {arn = a} :: DescribedConnector)

instance Core.FromJSON DescribedConnector where
  parseJSON =
    Core.withObject
      "DescribedConnector"
      ( \x ->
          DescribedConnector'
            Prelude.<$> (x Core..:? "Tags")
            Prelude.<*> (x Core..:? "ConnectorId")
            Prelude.<*> (x Core..:? "AccessRole")
            Prelude.<*> (x Core..:? "Url")
            Prelude.<*> (x Core..:? "As2Config")
            Prelude.<*> (x Core..:? "LoggingRole")
            Prelude.<*> (x Core..: "Arn")
      )

instance Prelude.Hashable DescribedConnector where
  hashWithSalt _salt DescribedConnector' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` connectorId
      `Prelude.hashWithSalt` accessRole
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` as2Config
      `Prelude.hashWithSalt` loggingRole
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribedConnector where
  rnf DescribedConnector' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf connectorId
      `Prelude.seq` Prelude.rnf accessRole
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf as2Config
      `Prelude.seq` Prelude.rnf loggingRole
      `Prelude.seq` Prelude.rnf arn
