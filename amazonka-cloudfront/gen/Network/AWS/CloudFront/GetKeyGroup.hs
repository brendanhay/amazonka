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
-- Module      : Network.AWS.CloudFront.GetKeyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key group, including the date and time when the key group was
-- last modified.
--
-- To get a key group, you must provide the key group’s identifier. If the
-- key group is referenced in a distribution’s cache behavior, you can get
-- the key group’s identifier using @ListDistributions@ or
-- @GetDistribution@. If the key group is not referenced in a cache
-- behavior, you can get the identifier using @ListKeyGroups@.
module Network.AWS.CloudFront.GetKeyGroup
  ( -- * Creating a Request
    GetKeyGroup (..),
    newGetKeyGroup,

    -- * Request Lenses
    getKeyGroup_id,

    -- * Destructuring the Response
    GetKeyGroupResponse (..),
    newGetKeyGroupResponse,

    -- * Response Lenses
    getKeyGroupResponse_eTag,
    getKeyGroupResponse_keyGroup,
    getKeyGroupResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetKeyGroup' smart constructor.
data GetKeyGroup = GetKeyGroup'
  { -- | The identifier of the key group that you are getting. To get the
    -- identifier, use @ListKeyGroups@.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetKeyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getKeyGroup_id' - The identifier of the key group that you are getting. To get the
-- identifier, use @ListKeyGroups@.
newGetKeyGroup ::
  -- | 'id'
  Core.Text ->
  GetKeyGroup
newGetKeyGroup pId_ = GetKeyGroup' {id = pId_}

-- | The identifier of the key group that you are getting. To get the
-- identifier, use @ListKeyGroups@.
getKeyGroup_id :: Lens.Lens' GetKeyGroup Core.Text
getKeyGroup_id = Lens.lens (\GetKeyGroup' {id} -> id) (\s@GetKeyGroup' {} a -> s {id = a} :: GetKeyGroup)

instance Core.AWSRequest GetKeyGroup where
  type AWSResponse GetKeyGroup = GetKeyGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetKeyGroupResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetKeyGroup

instance Core.NFData GetKeyGroup

instance Core.ToHeaders GetKeyGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetKeyGroup where
  toPath GetKeyGroup' {..} =
    Core.mconcat
      ["/2020-05-31/key-group/", Core.toBS id]

instance Core.ToQuery GetKeyGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetKeyGroupResponse' smart constructor.
data GetKeyGroupResponse = GetKeyGroupResponse'
  { -- | The identifier for this version of the key group.
    eTag :: Core.Maybe Core.Text,
    -- | The key group.
    keyGroup :: Core.Maybe KeyGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetKeyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getKeyGroupResponse_eTag' - The identifier for this version of the key group.
--
-- 'keyGroup', 'getKeyGroupResponse_keyGroup' - The key group.
--
-- 'httpStatus', 'getKeyGroupResponse_httpStatus' - The response's http status code.
newGetKeyGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetKeyGroupResponse
newGetKeyGroupResponse pHttpStatus_ =
  GetKeyGroupResponse'
    { eTag = Core.Nothing,
      keyGroup = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this version of the key group.
getKeyGroupResponse_eTag :: Lens.Lens' GetKeyGroupResponse (Core.Maybe Core.Text)
getKeyGroupResponse_eTag = Lens.lens (\GetKeyGroupResponse' {eTag} -> eTag) (\s@GetKeyGroupResponse' {} a -> s {eTag = a} :: GetKeyGroupResponse)

-- | The key group.
getKeyGroupResponse_keyGroup :: Lens.Lens' GetKeyGroupResponse (Core.Maybe KeyGroup)
getKeyGroupResponse_keyGroup = Lens.lens (\GetKeyGroupResponse' {keyGroup} -> keyGroup) (\s@GetKeyGroupResponse' {} a -> s {keyGroup = a} :: GetKeyGroupResponse)

-- | The response's http status code.
getKeyGroupResponse_httpStatus :: Lens.Lens' GetKeyGroupResponse Core.Int
getKeyGroupResponse_httpStatus = Lens.lens (\GetKeyGroupResponse' {httpStatus} -> httpStatus) (\s@GetKeyGroupResponse' {} a -> s {httpStatus = a} :: GetKeyGroupResponse)

instance Core.NFData GetKeyGroupResponse
