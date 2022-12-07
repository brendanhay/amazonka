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
-- Module      : Amazonka.CloudFront.GetKeyGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudFront.GetKeyGroup
  ( -- * Creating a Request
    GetKeyGroup (..),
    newGetKeyGroup,

    -- * Request Lenses
    getKeyGroup_id,

    -- * Destructuring the Response
    GetKeyGroupResponse (..),
    newGetKeyGroupResponse,

    -- * Response Lenses
    getKeyGroupResponse_keyGroup,
    getKeyGroupResponse_eTag,
    getKeyGroupResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKeyGroup' smart constructor.
data GetKeyGroup = GetKeyGroup'
  { -- | The identifier of the key group that you are getting. To get the
    -- identifier, use @ListKeyGroups@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetKeyGroup
newGetKeyGroup pId_ = GetKeyGroup' {id = pId_}

-- | The identifier of the key group that you are getting. To get the
-- identifier, use @ListKeyGroups@.
getKeyGroup_id :: Lens.Lens' GetKeyGroup Prelude.Text
getKeyGroup_id = Lens.lens (\GetKeyGroup' {id} -> id) (\s@GetKeyGroup' {} a -> s {id = a} :: GetKeyGroup)

instance Core.AWSRequest GetKeyGroup where
  type AWSResponse GetKeyGroup = GetKeyGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetKeyGroupResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKeyGroup where
  hashWithSalt _salt GetKeyGroup' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetKeyGroup where
  rnf GetKeyGroup' {..} = Prelude.rnf id

instance Data.ToHeaders GetKeyGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetKeyGroup where
  toPath GetKeyGroup' {..} =
    Prelude.mconcat
      ["/2020-05-31/key-group/", Data.toBS id]

instance Data.ToQuery GetKeyGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKeyGroupResponse' smart constructor.
data GetKeyGroupResponse = GetKeyGroupResponse'
  { -- | The key group.
    keyGroup :: Prelude.Maybe KeyGroup,
    -- | The identifier for this version of the key group.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyGroup', 'getKeyGroupResponse_keyGroup' - The key group.
--
-- 'eTag', 'getKeyGroupResponse_eTag' - The identifier for this version of the key group.
--
-- 'httpStatus', 'getKeyGroupResponse_httpStatus' - The response's http status code.
newGetKeyGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKeyGroupResponse
newGetKeyGroupResponse pHttpStatus_ =
  GetKeyGroupResponse'
    { keyGroup = Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The key group.
getKeyGroupResponse_keyGroup :: Lens.Lens' GetKeyGroupResponse (Prelude.Maybe KeyGroup)
getKeyGroupResponse_keyGroup = Lens.lens (\GetKeyGroupResponse' {keyGroup} -> keyGroup) (\s@GetKeyGroupResponse' {} a -> s {keyGroup = a} :: GetKeyGroupResponse)

-- | The identifier for this version of the key group.
getKeyGroupResponse_eTag :: Lens.Lens' GetKeyGroupResponse (Prelude.Maybe Prelude.Text)
getKeyGroupResponse_eTag = Lens.lens (\GetKeyGroupResponse' {eTag} -> eTag) (\s@GetKeyGroupResponse' {} a -> s {eTag = a} :: GetKeyGroupResponse)

-- | The response's http status code.
getKeyGroupResponse_httpStatus :: Lens.Lens' GetKeyGroupResponse Prelude.Int
getKeyGroupResponse_httpStatus = Lens.lens (\GetKeyGroupResponse' {httpStatus} -> httpStatus) (\s@GetKeyGroupResponse' {} a -> s {httpStatus = a} :: GetKeyGroupResponse)

instance Prelude.NFData GetKeyGroupResponse where
  rnf GetKeyGroupResponse' {..} =
    Prelude.rnf keyGroup
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
