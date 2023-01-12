{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaPackageVOD.ConfigureLogs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the packaging group\'s properities to configure log subscription
module Amazonka.MediaPackageVOD.ConfigureLogs
  ( -- * Creating a Request
    ConfigureLogs (..),
    newConfigureLogs,

    -- * Request Lenses
    configureLogs_egressAccessLogs,
    configureLogs_id,

    -- * Destructuring the Response
    ConfigureLogsResponse (..),
    newConfigureLogsResponse,

    -- * Response Lenses
    configureLogsResponse_arn,
    configureLogsResponse_authorization,
    configureLogsResponse_domainName,
    configureLogsResponse_egressAccessLogs,
    configureLogsResponse_id,
    configureLogsResponse_tags,
    configureLogsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The option to configure log subscription.
--
-- /See:/ 'newConfigureLogs' smart constructor.
data ConfigureLogs = ConfigureLogs'
  { egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    -- | The ID of a MediaPackage VOD PackagingGroup resource.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigureLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressAccessLogs', 'configureLogs_egressAccessLogs' - Undocumented member.
--
-- 'id', 'configureLogs_id' - The ID of a MediaPackage VOD PackagingGroup resource.
newConfigureLogs ::
  -- | 'id'
  Prelude.Text ->
  ConfigureLogs
newConfigureLogs pId_ =
  ConfigureLogs'
    { egressAccessLogs = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
configureLogs_egressAccessLogs :: Lens.Lens' ConfigureLogs (Prelude.Maybe EgressAccessLogs)
configureLogs_egressAccessLogs = Lens.lens (\ConfigureLogs' {egressAccessLogs} -> egressAccessLogs) (\s@ConfigureLogs' {} a -> s {egressAccessLogs = a} :: ConfigureLogs)

-- | The ID of a MediaPackage VOD PackagingGroup resource.
configureLogs_id :: Lens.Lens' ConfigureLogs Prelude.Text
configureLogs_id = Lens.lens (\ConfigureLogs' {id} -> id) (\s@ConfigureLogs' {} a -> s {id = a} :: ConfigureLogs)

instance Core.AWSRequest ConfigureLogs where
  type
    AWSResponse ConfigureLogs =
      ConfigureLogsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfigureLogsResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authorization")
            Prelude.<*> (x Data..?> "domainName")
            Prelude.<*> (x Data..?> "egressAccessLogs")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfigureLogs where
  hashWithSalt _salt ConfigureLogs' {..} =
    _salt `Prelude.hashWithSalt` egressAccessLogs
      `Prelude.hashWithSalt` id

instance Prelude.NFData ConfigureLogs where
  rnf ConfigureLogs' {..} =
    Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders ConfigureLogs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConfigureLogs where
  toJSON ConfigureLogs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("egressAccessLogs" Data..=)
              Prelude.<$> egressAccessLogs
          ]
      )

instance Data.ToPath ConfigureLogs where
  toPath ConfigureLogs' {..} =
    Prelude.mconcat
      [ "/packaging_groups/",
        Data.toBS id,
        "/configure_logs"
      ]

instance Data.ToQuery ConfigureLogs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConfigureLogsResponse' smart constructor.
data ConfigureLogsResponse = ConfigureLogsResponse'
  { -- | The ARN of the PackagingGroup.
    arn :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    -- | The fully qualified domain name for Assets in the PackagingGroup.
    domainName :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    -- | The ID of the PackagingGroup.
    id :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigureLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'configureLogsResponse_arn' - The ARN of the PackagingGroup.
--
-- 'authorization', 'configureLogsResponse_authorization' - Undocumented member.
--
-- 'domainName', 'configureLogsResponse_domainName' - The fully qualified domain name for Assets in the PackagingGroup.
--
-- 'egressAccessLogs', 'configureLogsResponse_egressAccessLogs' - Undocumented member.
--
-- 'id', 'configureLogsResponse_id' - The ID of the PackagingGroup.
--
-- 'tags', 'configureLogsResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'configureLogsResponse_httpStatus' - The response's http status code.
newConfigureLogsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfigureLogsResponse
newConfigureLogsResponse pHttpStatus_ =
  ConfigureLogsResponse'
    { arn = Prelude.Nothing,
      authorization = Prelude.Nothing,
      domainName = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      id = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the PackagingGroup.
configureLogsResponse_arn :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe Prelude.Text)
configureLogsResponse_arn = Lens.lens (\ConfigureLogsResponse' {arn} -> arn) (\s@ConfigureLogsResponse' {} a -> s {arn = a} :: ConfigureLogsResponse)

-- | Undocumented member.
configureLogsResponse_authorization :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe Authorization)
configureLogsResponse_authorization = Lens.lens (\ConfigureLogsResponse' {authorization} -> authorization) (\s@ConfigureLogsResponse' {} a -> s {authorization = a} :: ConfigureLogsResponse)

-- | The fully qualified domain name for Assets in the PackagingGroup.
configureLogsResponse_domainName :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe Prelude.Text)
configureLogsResponse_domainName = Lens.lens (\ConfigureLogsResponse' {domainName} -> domainName) (\s@ConfigureLogsResponse' {} a -> s {domainName = a} :: ConfigureLogsResponse)

-- | Undocumented member.
configureLogsResponse_egressAccessLogs :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe EgressAccessLogs)
configureLogsResponse_egressAccessLogs = Lens.lens (\ConfigureLogsResponse' {egressAccessLogs} -> egressAccessLogs) (\s@ConfigureLogsResponse' {} a -> s {egressAccessLogs = a} :: ConfigureLogsResponse)

-- | The ID of the PackagingGroup.
configureLogsResponse_id :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe Prelude.Text)
configureLogsResponse_id = Lens.lens (\ConfigureLogsResponse' {id} -> id) (\s@ConfigureLogsResponse' {} a -> s {id = a} :: ConfigureLogsResponse)

-- | Undocumented member.
configureLogsResponse_tags :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configureLogsResponse_tags = Lens.lens (\ConfigureLogsResponse' {tags} -> tags) (\s@ConfigureLogsResponse' {} a -> s {tags = a} :: ConfigureLogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
configureLogsResponse_httpStatus :: Lens.Lens' ConfigureLogsResponse Prelude.Int
configureLogsResponse_httpStatus = Lens.lens (\ConfigureLogsResponse' {httpStatus} -> httpStatus) (\s@ConfigureLogsResponse' {} a -> s {httpStatus = a} :: ConfigureLogsResponse)

instance Prelude.NFData ConfigureLogsResponse where
  rnf ConfigureLogsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authorization
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
