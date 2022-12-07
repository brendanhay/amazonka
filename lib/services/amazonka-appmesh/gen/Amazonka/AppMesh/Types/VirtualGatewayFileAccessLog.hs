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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayFileAccessLog
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayFileAccessLog where

import Amazonka.AppMesh.Types.LoggingFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an access log file.
--
-- /See:/ 'newVirtualGatewayFileAccessLog' smart constructor.
data VirtualGatewayFileAccessLog = VirtualGatewayFileAccessLog'
  { -- | The specified format for the virtual gateway access logs. It can be
    -- either @json_format@ or @text_format@.
    format :: Prelude.Maybe LoggingFormat,
    -- | The file path to write access logs to. You can use @\/dev\/stdout@ to
    -- send access logs to standard out and configure your Envoy container to
    -- use a log driver, such as @awslogs@, to export the access logs to a log
    -- storage service such as Amazon CloudWatch Logs. You can also specify a
    -- path in the Envoy container\'s file system to write the files to disk.
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayFileAccessLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'virtualGatewayFileAccessLog_format' - The specified format for the virtual gateway access logs. It can be
-- either @json_format@ or @text_format@.
--
-- 'path', 'virtualGatewayFileAccessLog_path' - The file path to write access logs to. You can use @\/dev\/stdout@ to
-- send access logs to standard out and configure your Envoy container to
-- use a log driver, such as @awslogs@, to export the access logs to a log
-- storage service such as Amazon CloudWatch Logs. You can also specify a
-- path in the Envoy container\'s file system to write the files to disk.
newVirtualGatewayFileAccessLog ::
  -- | 'path'
  Prelude.Text ->
  VirtualGatewayFileAccessLog
newVirtualGatewayFileAccessLog pPath_ =
  VirtualGatewayFileAccessLog'
    { format =
        Prelude.Nothing,
      path = pPath_
    }

-- | The specified format for the virtual gateway access logs. It can be
-- either @json_format@ or @text_format@.
virtualGatewayFileAccessLog_format :: Lens.Lens' VirtualGatewayFileAccessLog (Prelude.Maybe LoggingFormat)
virtualGatewayFileAccessLog_format = Lens.lens (\VirtualGatewayFileAccessLog' {format} -> format) (\s@VirtualGatewayFileAccessLog' {} a -> s {format = a} :: VirtualGatewayFileAccessLog)

-- | The file path to write access logs to. You can use @\/dev\/stdout@ to
-- send access logs to standard out and configure your Envoy container to
-- use a log driver, such as @awslogs@, to export the access logs to a log
-- storage service such as Amazon CloudWatch Logs. You can also specify a
-- path in the Envoy container\'s file system to write the files to disk.
virtualGatewayFileAccessLog_path :: Lens.Lens' VirtualGatewayFileAccessLog Prelude.Text
virtualGatewayFileAccessLog_path = Lens.lens (\VirtualGatewayFileAccessLog' {path} -> path) (\s@VirtualGatewayFileAccessLog' {} a -> s {path = a} :: VirtualGatewayFileAccessLog)

instance Data.FromJSON VirtualGatewayFileAccessLog where
  parseJSON =
    Data.withObject
      "VirtualGatewayFileAccessLog"
      ( \x ->
          VirtualGatewayFileAccessLog'
            Prelude.<$> (x Data..:? "format") Prelude.<*> (x Data..: "path")
      )

instance Prelude.Hashable VirtualGatewayFileAccessLog where
  hashWithSalt _salt VirtualGatewayFileAccessLog' {..} =
    _salt `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` path

instance Prelude.NFData VirtualGatewayFileAccessLog where
  rnf VirtualGatewayFileAccessLog' {..} =
    Prelude.rnf format `Prelude.seq` Prelude.rnf path

instance Data.ToJSON VirtualGatewayFileAccessLog where
  toJSON VirtualGatewayFileAccessLog' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("format" Data..=) Prelude.<$> format,
            Prelude.Just ("path" Data..= path)
          ]
      )
