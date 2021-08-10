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
-- Module      : Network.AWS.CloudFront.GetKeyGroupConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key group configuration.
--
-- To get a key group configuration, you must provide the key group’s
-- identifier. If the key group is referenced in a distribution’s cache
-- behavior, you can get the key group’s identifier using
-- @ListDistributions@ or @GetDistribution@. If the key group is not
-- referenced in a cache behavior, you can get the identifier using
-- @ListKeyGroups@.
module Network.AWS.CloudFront.GetKeyGroupConfig
  ( -- * Creating a Request
    GetKeyGroupConfig (..),
    newGetKeyGroupConfig,

    -- * Request Lenses
    getKeyGroupConfig_id,

    -- * Destructuring the Response
    GetKeyGroupConfigResponse (..),
    newGetKeyGroupConfigResponse,

    -- * Response Lenses
    getKeyGroupConfigResponse_eTag,
    getKeyGroupConfigResponse_keyGroupConfig,
    getKeyGroupConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetKeyGroupConfig' smart constructor.
data GetKeyGroupConfig = GetKeyGroupConfig'
  { -- | The identifier of the key group whose configuration you are getting. To
    -- get the identifier, use @ListKeyGroups@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getKeyGroupConfig_id' - The identifier of the key group whose configuration you are getting. To
-- get the identifier, use @ListKeyGroups@.
newGetKeyGroupConfig ::
  -- | 'id'
  Prelude.Text ->
  GetKeyGroupConfig
newGetKeyGroupConfig pId_ =
  GetKeyGroupConfig' {id = pId_}

-- | The identifier of the key group whose configuration you are getting. To
-- get the identifier, use @ListKeyGroups@.
getKeyGroupConfig_id :: Lens.Lens' GetKeyGroupConfig Prelude.Text
getKeyGroupConfig_id = Lens.lens (\GetKeyGroupConfig' {id} -> id) (\s@GetKeyGroupConfig' {} a -> s {id = a} :: GetKeyGroupConfig)

instance Core.AWSRequest GetKeyGroupConfig where
  type
    AWSResponse GetKeyGroupConfig =
      GetKeyGroupConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetKeyGroupConfigResponse'
            Prelude.<$> (h Core..#? "ETag")
            Prelude.<*> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKeyGroupConfig

instance Prelude.NFData GetKeyGroupConfig

instance Core.ToHeaders GetKeyGroupConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetKeyGroupConfig where
  toPath GetKeyGroupConfig' {..} =
    Prelude.mconcat
      ["/2020-05-31/key-group/", Core.toBS id, "/config"]

instance Core.ToQuery GetKeyGroupConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKeyGroupConfigResponse' smart constructor.
data GetKeyGroupConfigResponse = GetKeyGroupConfigResponse'
  { -- | The identifier for this version of the key group.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The key group configuration.
    keyGroupConfig :: Prelude.Maybe KeyGroupConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyGroupConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getKeyGroupConfigResponse_eTag' - The identifier for this version of the key group.
--
-- 'keyGroupConfig', 'getKeyGroupConfigResponse_keyGroupConfig' - The key group configuration.
--
-- 'httpStatus', 'getKeyGroupConfigResponse_httpStatus' - The response's http status code.
newGetKeyGroupConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKeyGroupConfigResponse
newGetKeyGroupConfigResponse pHttpStatus_ =
  GetKeyGroupConfigResponse'
    { eTag = Prelude.Nothing,
      keyGroupConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this version of the key group.
getKeyGroupConfigResponse_eTag :: Lens.Lens' GetKeyGroupConfigResponse (Prelude.Maybe Prelude.Text)
getKeyGroupConfigResponse_eTag = Lens.lens (\GetKeyGroupConfigResponse' {eTag} -> eTag) (\s@GetKeyGroupConfigResponse' {} a -> s {eTag = a} :: GetKeyGroupConfigResponse)

-- | The key group configuration.
getKeyGroupConfigResponse_keyGroupConfig :: Lens.Lens' GetKeyGroupConfigResponse (Prelude.Maybe KeyGroupConfig)
getKeyGroupConfigResponse_keyGroupConfig = Lens.lens (\GetKeyGroupConfigResponse' {keyGroupConfig} -> keyGroupConfig) (\s@GetKeyGroupConfigResponse' {} a -> s {keyGroupConfig = a} :: GetKeyGroupConfigResponse)

-- | The response's http status code.
getKeyGroupConfigResponse_httpStatus :: Lens.Lens' GetKeyGroupConfigResponse Prelude.Int
getKeyGroupConfigResponse_httpStatus = Lens.lens (\GetKeyGroupConfigResponse' {httpStatus} -> httpStatus) (\s@GetKeyGroupConfigResponse' {} a -> s {httpStatus = a} :: GetKeyGroupConfigResponse)

instance Prelude.NFData GetKeyGroupConfigResponse
