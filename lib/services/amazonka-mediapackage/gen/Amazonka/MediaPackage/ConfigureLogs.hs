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
-- Module      : Amazonka.MediaPackage.ConfigureLogs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Channel\'s properities to configure log subscription
module Amazonka.MediaPackage.ConfigureLogs
  ( -- * Creating a Request
    ConfigureLogs (..),
    newConfigureLogs,

    -- * Request Lenses
    configureLogs_ingressAccessLogs,
    configureLogs_egressAccessLogs,
    configureLogs_id,

    -- * Destructuring the Response
    ConfigureLogsResponse (..),
    newConfigureLogsResponse,

    -- * Response Lenses
    configureLogsResponse_tags,
    configureLogsResponse_ingressAccessLogs,
    configureLogsResponse_arn,
    configureLogsResponse_id,
    configureLogsResponse_description,
    configureLogsResponse_egressAccessLogs,
    configureLogsResponse_hlsIngest,
    configureLogsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | the option to configure log subscription.
--
-- /See:/ 'newConfigureLogs' smart constructor.
data ConfigureLogs = ConfigureLogs'
  { ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    -- | The ID of the channel to log subscription.
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
-- 'ingressAccessLogs', 'configureLogs_ingressAccessLogs' - Undocumented member.
--
-- 'egressAccessLogs', 'configureLogs_egressAccessLogs' - Undocumented member.
--
-- 'id', 'configureLogs_id' - The ID of the channel to log subscription.
newConfigureLogs ::
  -- | 'id'
  Prelude.Text ->
  ConfigureLogs
newConfigureLogs pId_ =
  ConfigureLogs'
    { ingressAccessLogs = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
configureLogs_ingressAccessLogs :: Lens.Lens' ConfigureLogs (Prelude.Maybe IngressAccessLogs)
configureLogs_ingressAccessLogs = Lens.lens (\ConfigureLogs' {ingressAccessLogs} -> ingressAccessLogs) (\s@ConfigureLogs' {} a -> s {ingressAccessLogs = a} :: ConfigureLogs)

-- | Undocumented member.
configureLogs_egressAccessLogs :: Lens.Lens' ConfigureLogs (Prelude.Maybe EgressAccessLogs)
configureLogs_egressAccessLogs = Lens.lens (\ConfigureLogs' {egressAccessLogs} -> egressAccessLogs) (\s@ConfigureLogs' {} a -> s {egressAccessLogs = a} :: ConfigureLogs)

-- | The ID of the channel to log subscription.
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
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ingressAccessLogs")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "egressAccessLogs")
            Prelude.<*> (x Data..?> "hlsIngest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfigureLogs where
  hashWithSalt _salt ConfigureLogs' {..} =
    _salt `Prelude.hashWithSalt` ingressAccessLogs
      `Prelude.hashWithSalt` egressAccessLogs
      `Prelude.hashWithSalt` id

instance Prelude.NFData ConfigureLogs where
  rnf ConfigureLogs' {..} =
    Prelude.rnf ingressAccessLogs
      `Prelude.seq` Prelude.rnf egressAccessLogs
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
          [ ("ingressAccessLogs" Data..=)
              Prelude.<$> ingressAccessLogs,
            ("egressAccessLogs" Data..=)
              Prelude.<$> egressAccessLogs
          ]
      )

instance Data.ToPath ConfigureLogs where
  toPath ConfigureLogs' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS id, "/configure_logs"]

instance Data.ToQuery ConfigureLogs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConfigureLogsResponse' smart constructor.
data ConfigureLogsResponse = ConfigureLogsResponse'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel.
    id :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    hlsIngest :: Prelude.Maybe HlsIngest,
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
-- 'tags', 'configureLogsResponse_tags' - Undocumented member.
--
-- 'ingressAccessLogs', 'configureLogsResponse_ingressAccessLogs' - Undocumented member.
--
-- 'arn', 'configureLogsResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'configureLogsResponse_id' - The ID of the Channel.
--
-- 'description', 'configureLogsResponse_description' - A short text description of the Channel.
--
-- 'egressAccessLogs', 'configureLogsResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'configureLogsResponse_hlsIngest' - Undocumented member.
--
-- 'httpStatus', 'configureLogsResponse_httpStatus' - The response's http status code.
newConfigureLogsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfigureLogsResponse
newConfigureLogsResponse pHttpStatus_ =
  ConfigureLogsResponse'
    { tags = Prelude.Nothing,
      ingressAccessLogs = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      hlsIngest = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
configureLogsResponse_tags :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configureLogsResponse_tags = Lens.lens (\ConfigureLogsResponse' {tags} -> tags) (\s@ConfigureLogsResponse' {} a -> s {tags = a} :: ConfigureLogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
configureLogsResponse_ingressAccessLogs :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe IngressAccessLogs)
configureLogsResponse_ingressAccessLogs = Lens.lens (\ConfigureLogsResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@ConfigureLogsResponse' {} a -> s {ingressAccessLogs = a} :: ConfigureLogsResponse)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
configureLogsResponse_arn :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe Prelude.Text)
configureLogsResponse_arn = Lens.lens (\ConfigureLogsResponse' {arn} -> arn) (\s@ConfigureLogsResponse' {} a -> s {arn = a} :: ConfigureLogsResponse)

-- | The ID of the Channel.
configureLogsResponse_id :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe Prelude.Text)
configureLogsResponse_id = Lens.lens (\ConfigureLogsResponse' {id} -> id) (\s@ConfigureLogsResponse' {} a -> s {id = a} :: ConfigureLogsResponse)

-- | A short text description of the Channel.
configureLogsResponse_description :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe Prelude.Text)
configureLogsResponse_description = Lens.lens (\ConfigureLogsResponse' {description} -> description) (\s@ConfigureLogsResponse' {} a -> s {description = a} :: ConfigureLogsResponse)

-- | Undocumented member.
configureLogsResponse_egressAccessLogs :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe EgressAccessLogs)
configureLogsResponse_egressAccessLogs = Lens.lens (\ConfigureLogsResponse' {egressAccessLogs} -> egressAccessLogs) (\s@ConfigureLogsResponse' {} a -> s {egressAccessLogs = a} :: ConfigureLogsResponse)

-- | Undocumented member.
configureLogsResponse_hlsIngest :: Lens.Lens' ConfigureLogsResponse (Prelude.Maybe HlsIngest)
configureLogsResponse_hlsIngest = Lens.lens (\ConfigureLogsResponse' {hlsIngest} -> hlsIngest) (\s@ConfigureLogsResponse' {} a -> s {hlsIngest = a} :: ConfigureLogsResponse)

-- | The response's http status code.
configureLogsResponse_httpStatus :: Lens.Lens' ConfigureLogsResponse Prelude.Int
configureLogsResponse_httpStatus = Lens.lens (\ConfigureLogsResponse' {httpStatus} -> httpStatus) (\s@ConfigureLogsResponse' {} a -> s {httpStatus = a} :: ConfigureLogsResponse)

instance Prelude.NFData ConfigureLogsResponse where
  rnf ConfigureLogsResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ingressAccessLogs
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf hlsIngest
      `Prelude.seq` Prelude.rnf httpStatus
