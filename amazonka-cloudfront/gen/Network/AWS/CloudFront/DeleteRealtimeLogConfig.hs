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
-- Module      : Network.AWS.CloudFront.DeleteRealtimeLogConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real-time log configuration.
--
-- You cannot delete a real-time log configuration if it’s attached to a
-- cache behavior. First update your distributions to remove the real-time
-- log configuration from all cache behaviors, then delete the real-time
-- log configuration.
--
-- To delete a real-time log configuration, you can provide the
-- configuration’s name or its Amazon Resource Name (ARN). You must provide
-- at least one. If you provide both, CloudFront uses the name to identify
-- the real-time log configuration to delete.
module Network.AWS.CloudFront.DeleteRealtimeLogConfig
  ( -- * Creating a Request
    DeleteRealtimeLogConfig (..),
    newDeleteRealtimeLogConfig,

    -- * Request Lenses
    deleteRealtimeLogConfig_arn,
    deleteRealtimeLogConfig_name,

    -- * Destructuring the Response
    DeleteRealtimeLogConfigResponse (..),
    newDeleteRealtimeLogConfigResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRealtimeLogConfig' smart constructor.
data DeleteRealtimeLogConfig = DeleteRealtimeLogConfig'
  { -- | The Amazon Resource Name (ARN) of the real-time log configuration to
    -- delete.
    arn :: Core.Maybe Core.Text,
    -- | The name of the real-time log configuration to delete.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRealtimeLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteRealtimeLogConfig_arn' - The Amazon Resource Name (ARN) of the real-time log configuration to
-- delete.
--
-- 'name', 'deleteRealtimeLogConfig_name' - The name of the real-time log configuration to delete.
newDeleteRealtimeLogConfig ::
  DeleteRealtimeLogConfig
newDeleteRealtimeLogConfig =
  DeleteRealtimeLogConfig'
    { arn = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the real-time log configuration to
-- delete.
deleteRealtimeLogConfig_arn :: Lens.Lens' DeleteRealtimeLogConfig (Core.Maybe Core.Text)
deleteRealtimeLogConfig_arn = Lens.lens (\DeleteRealtimeLogConfig' {arn} -> arn) (\s@DeleteRealtimeLogConfig' {} a -> s {arn = a} :: DeleteRealtimeLogConfig)

-- | The name of the real-time log configuration to delete.
deleteRealtimeLogConfig_name :: Lens.Lens' DeleteRealtimeLogConfig (Core.Maybe Core.Text)
deleteRealtimeLogConfig_name = Lens.lens (\DeleteRealtimeLogConfig' {name} -> name) (\s@DeleteRealtimeLogConfig' {} a -> s {name = a} :: DeleteRealtimeLogConfig)

instance Core.AWSRequest DeleteRealtimeLogConfig where
  type
    AWSResponse DeleteRealtimeLogConfig =
      DeleteRealtimeLogConfigResponse
  request = Request.postXML defaultService
  response =
    Response.receiveNull
      DeleteRealtimeLogConfigResponse'

instance Core.Hashable DeleteRealtimeLogConfig

instance Core.NFData DeleteRealtimeLogConfig

instance Core.ToElement DeleteRealtimeLogConfig where
  toElement =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DeleteRealtimeLogConfigRequest"

instance Core.ToHeaders DeleteRealtimeLogConfig where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteRealtimeLogConfig where
  toPath =
    Core.const
      "/2020-05-31/delete-realtime-log-config/"

instance Core.ToQuery DeleteRealtimeLogConfig where
  toQuery = Core.const Core.mempty

instance Core.ToXML DeleteRealtimeLogConfig where
  toXML DeleteRealtimeLogConfig' {..} =
    Core.mconcat
      ["ARN" Core.@= arn, "Name" Core.@= name]

-- | /See:/ 'newDeleteRealtimeLogConfigResponse' smart constructor.
data DeleteRealtimeLogConfigResponse = DeleteRealtimeLogConfigResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRealtimeLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRealtimeLogConfigResponse ::
  DeleteRealtimeLogConfigResponse
newDeleteRealtimeLogConfigResponse =
  DeleteRealtimeLogConfigResponse'

instance Core.NFData DeleteRealtimeLogConfigResponse
