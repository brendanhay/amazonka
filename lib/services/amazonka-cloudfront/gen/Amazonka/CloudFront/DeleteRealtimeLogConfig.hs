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
-- Module      : Amazonka.CloudFront.DeleteRealtimeLogConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudFront.DeleteRealtimeLogConfig
  ( -- * Creating a Request
    DeleteRealtimeLogConfig (..),
    newDeleteRealtimeLogConfig,

    -- * Request Lenses
    deleteRealtimeLogConfig_name,
    deleteRealtimeLogConfig_arn,

    -- * Destructuring the Response
    DeleteRealtimeLogConfigResponse (..),
    newDeleteRealtimeLogConfigResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRealtimeLogConfig' smart constructor.
data DeleteRealtimeLogConfig = DeleteRealtimeLogConfig'
  { -- | The name of the real-time log configuration to delete.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the real-time log configuration to
    -- delete.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRealtimeLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteRealtimeLogConfig_name' - The name of the real-time log configuration to delete.
--
-- 'arn', 'deleteRealtimeLogConfig_arn' - The Amazon Resource Name (ARN) of the real-time log configuration to
-- delete.
newDeleteRealtimeLogConfig ::
  DeleteRealtimeLogConfig
newDeleteRealtimeLogConfig =
  DeleteRealtimeLogConfig'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The name of the real-time log configuration to delete.
deleteRealtimeLogConfig_name :: Lens.Lens' DeleteRealtimeLogConfig (Prelude.Maybe Prelude.Text)
deleteRealtimeLogConfig_name = Lens.lens (\DeleteRealtimeLogConfig' {name} -> name) (\s@DeleteRealtimeLogConfig' {} a -> s {name = a} :: DeleteRealtimeLogConfig)

-- | The Amazon Resource Name (ARN) of the real-time log configuration to
-- delete.
deleteRealtimeLogConfig_arn :: Lens.Lens' DeleteRealtimeLogConfig (Prelude.Maybe Prelude.Text)
deleteRealtimeLogConfig_arn = Lens.lens (\DeleteRealtimeLogConfig' {arn} -> arn) (\s@DeleteRealtimeLogConfig' {} a -> s {arn = a} :: DeleteRealtimeLogConfig)

instance Core.AWSRequest DeleteRealtimeLogConfig where
  type
    AWSResponse DeleteRealtimeLogConfig =
      DeleteRealtimeLogConfigResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveNull
      DeleteRealtimeLogConfigResponse'

instance Prelude.Hashable DeleteRealtimeLogConfig where
  hashWithSalt _salt DeleteRealtimeLogConfig' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteRealtimeLogConfig where
  rnf DeleteRealtimeLogConfig' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf arn

instance Data.ToElement DeleteRealtimeLogConfig where
  toElement =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DeleteRealtimeLogConfigRequest"

instance Data.ToHeaders DeleteRealtimeLogConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteRealtimeLogConfig where
  toPath =
    Prelude.const
      "/2020-05-31/delete-realtime-log-config/"

instance Data.ToQuery DeleteRealtimeLogConfig where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML DeleteRealtimeLogConfig where
  toXML DeleteRealtimeLogConfig' {..} =
    Prelude.mconcat
      ["Name" Data.@= name, "ARN" Data.@= arn]

-- | /See:/ 'newDeleteRealtimeLogConfigResponse' smart constructor.
data DeleteRealtimeLogConfigResponse = DeleteRealtimeLogConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRealtimeLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRealtimeLogConfigResponse ::
  DeleteRealtimeLogConfigResponse
newDeleteRealtimeLogConfigResponse =
  DeleteRealtimeLogConfigResponse'

instance
  Prelude.NFData
    DeleteRealtimeLogConfigResponse
  where
  rnf _ = ()
