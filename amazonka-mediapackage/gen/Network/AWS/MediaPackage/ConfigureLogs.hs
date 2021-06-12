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
-- Module      : Network.AWS.MediaPackage.ConfigureLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Channel\'s properities to configure log subscription
module Network.AWS.MediaPackage.ConfigureLogs
  ( -- * Creating a Request
    ConfigureLogs (..),
    newConfigureLogs,

    -- * Request Lenses
    configureLogs_egressAccessLogs,
    configureLogs_ingressAccessLogs,
    configureLogs_id,

    -- * Destructuring the Response
    ConfigureLogsResponse (..),
    newConfigureLogsResponse,

    -- * Response Lenses
    configureLogsResponse_egressAccessLogs,
    configureLogsResponse_hlsIngest,
    configureLogsResponse_arn,
    configureLogsResponse_id,
    configureLogsResponse_ingressAccessLogs,
    configureLogsResponse_tags,
    configureLogsResponse_description,
    configureLogsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | the option to configure log subscription.
--
-- /See:/ 'newConfigureLogs' smart constructor.
data ConfigureLogs = ConfigureLogs'
  { egressAccessLogs :: Core.Maybe EgressAccessLogs,
    ingressAccessLogs :: Core.Maybe IngressAccessLogs,
    -- | The ID of the channel to log subscription.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'ingressAccessLogs', 'configureLogs_ingressAccessLogs' - Undocumented member.
--
-- 'id', 'configureLogs_id' - The ID of the channel to log subscription.
newConfigureLogs ::
  -- | 'id'
  Core.Text ->
  ConfigureLogs
newConfigureLogs pId_ =
  ConfigureLogs'
    { egressAccessLogs = Core.Nothing,
      ingressAccessLogs = Core.Nothing,
      id = pId_
    }

-- | Undocumented member.
configureLogs_egressAccessLogs :: Lens.Lens' ConfigureLogs (Core.Maybe EgressAccessLogs)
configureLogs_egressAccessLogs = Lens.lens (\ConfigureLogs' {egressAccessLogs} -> egressAccessLogs) (\s@ConfigureLogs' {} a -> s {egressAccessLogs = a} :: ConfigureLogs)

-- | Undocumented member.
configureLogs_ingressAccessLogs :: Lens.Lens' ConfigureLogs (Core.Maybe IngressAccessLogs)
configureLogs_ingressAccessLogs = Lens.lens (\ConfigureLogs' {ingressAccessLogs} -> ingressAccessLogs) (\s@ConfigureLogs' {} a -> s {ingressAccessLogs = a} :: ConfigureLogs)

-- | The ID of the channel to log subscription.
configureLogs_id :: Lens.Lens' ConfigureLogs Core.Text
configureLogs_id = Lens.lens (\ConfigureLogs' {id} -> id) (\s@ConfigureLogs' {} a -> s {id = a} :: ConfigureLogs)

instance Core.AWSRequest ConfigureLogs where
  type
    AWSResponse ConfigureLogs =
      ConfigureLogsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfigureLogsResponse'
            Core.<$> (x Core..?> "egressAccessLogs")
            Core.<*> (x Core..?> "hlsIngest")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "ingressAccessLogs")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ConfigureLogs

instance Core.NFData ConfigureLogs

instance Core.ToHeaders ConfigureLogs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ConfigureLogs where
  toJSON ConfigureLogs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("egressAccessLogs" Core..=)
              Core.<$> egressAccessLogs,
            ("ingressAccessLogs" Core..=)
              Core.<$> ingressAccessLogs
          ]
      )

instance Core.ToPath ConfigureLogs where
  toPath ConfigureLogs' {..} =
    Core.mconcat
      ["/channels/", Core.toBS id, "/configure_logs"]

instance Core.ToQuery ConfigureLogs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newConfigureLogsResponse' smart constructor.
data ConfigureLogsResponse = ConfigureLogsResponse'
  { egressAccessLogs :: Core.Maybe EgressAccessLogs,
    hlsIngest :: Core.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the Channel.
    id :: Core.Maybe Core.Text,
    ingressAccessLogs :: Core.Maybe IngressAccessLogs,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A short text description of the Channel.
    description :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigureLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressAccessLogs', 'configureLogsResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'configureLogsResponse_hlsIngest' - Undocumented member.
--
-- 'arn', 'configureLogsResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'configureLogsResponse_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'configureLogsResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'configureLogsResponse_tags' - Undocumented member.
--
-- 'description', 'configureLogsResponse_description' - A short text description of the Channel.
--
-- 'httpStatus', 'configureLogsResponse_httpStatus' - The response's http status code.
newConfigureLogsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ConfigureLogsResponse
newConfigureLogsResponse pHttpStatus_ =
  ConfigureLogsResponse'
    { egressAccessLogs =
        Core.Nothing,
      hlsIngest = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      ingressAccessLogs = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
configureLogsResponse_egressAccessLogs :: Lens.Lens' ConfigureLogsResponse (Core.Maybe EgressAccessLogs)
configureLogsResponse_egressAccessLogs = Lens.lens (\ConfigureLogsResponse' {egressAccessLogs} -> egressAccessLogs) (\s@ConfigureLogsResponse' {} a -> s {egressAccessLogs = a} :: ConfigureLogsResponse)

-- | Undocumented member.
configureLogsResponse_hlsIngest :: Lens.Lens' ConfigureLogsResponse (Core.Maybe HlsIngest)
configureLogsResponse_hlsIngest = Lens.lens (\ConfigureLogsResponse' {hlsIngest} -> hlsIngest) (\s@ConfigureLogsResponse' {} a -> s {hlsIngest = a} :: ConfigureLogsResponse)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
configureLogsResponse_arn :: Lens.Lens' ConfigureLogsResponse (Core.Maybe Core.Text)
configureLogsResponse_arn = Lens.lens (\ConfigureLogsResponse' {arn} -> arn) (\s@ConfigureLogsResponse' {} a -> s {arn = a} :: ConfigureLogsResponse)

-- | The ID of the Channel.
configureLogsResponse_id :: Lens.Lens' ConfigureLogsResponse (Core.Maybe Core.Text)
configureLogsResponse_id = Lens.lens (\ConfigureLogsResponse' {id} -> id) (\s@ConfigureLogsResponse' {} a -> s {id = a} :: ConfigureLogsResponse)

-- | Undocumented member.
configureLogsResponse_ingressAccessLogs :: Lens.Lens' ConfigureLogsResponse (Core.Maybe IngressAccessLogs)
configureLogsResponse_ingressAccessLogs = Lens.lens (\ConfigureLogsResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@ConfigureLogsResponse' {} a -> s {ingressAccessLogs = a} :: ConfigureLogsResponse)

-- | Undocumented member.
configureLogsResponse_tags :: Lens.Lens' ConfigureLogsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
configureLogsResponse_tags = Lens.lens (\ConfigureLogsResponse' {tags} -> tags) (\s@ConfigureLogsResponse' {} a -> s {tags = a} :: ConfigureLogsResponse) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the Channel.
configureLogsResponse_description :: Lens.Lens' ConfigureLogsResponse (Core.Maybe Core.Text)
configureLogsResponse_description = Lens.lens (\ConfigureLogsResponse' {description} -> description) (\s@ConfigureLogsResponse' {} a -> s {description = a} :: ConfigureLogsResponse)

-- | The response's http status code.
configureLogsResponse_httpStatus :: Lens.Lens' ConfigureLogsResponse Core.Int
configureLogsResponse_httpStatus = Lens.lens (\ConfigureLogsResponse' {httpStatus} -> httpStatus) (\s@ConfigureLogsResponse' {} a -> s {httpStatus = a} :: ConfigureLogsResponse)

instance Core.NFData ConfigureLogsResponse
