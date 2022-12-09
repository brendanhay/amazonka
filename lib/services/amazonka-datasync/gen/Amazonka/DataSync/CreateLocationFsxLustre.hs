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
-- Module      : Amazonka.DataSync.CreateLocationFsxLustre
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for an Amazon FSx for Lustre file system.
module Amazonka.DataSync.CreateLocationFsxLustre
  ( -- * Creating a Request
    CreateLocationFsxLustre (..),
    newCreateLocationFsxLustre,

    -- * Request Lenses
    createLocationFsxLustre_subdirectory,
    createLocationFsxLustre_tags,
    createLocationFsxLustre_fsxFilesystemArn,
    createLocationFsxLustre_securityGroupArns,

    -- * Destructuring the Response
    CreateLocationFsxLustreResponse (..),
    newCreateLocationFsxLustreResponse,

    -- * Response Lenses
    createLocationFsxLustreResponse_locationArn,
    createLocationFsxLustreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLocationFsxLustre' smart constructor.
data CreateLocationFsxLustre = CreateLocationFsxLustre'
  { -- | A subdirectory in the location\'s path. This subdirectory in the FSx for
    -- Lustre file system is used to read data from the FSx for Lustre source
    -- location or write data to the FSx for Lustre destination.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | The key-value pair that represents a tag that you want to add to the
    -- resource. The value can be an empty string. This value helps you manage,
    -- filter, and search for your resources. We recommend that you create a
    -- name tag for your location.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The Amazon Resource Name (ARN) for the FSx for Lustre file system.
    fsxFilesystemArn :: Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the security groups that are used to
    -- configure the FSx for Lustre file system.
    securityGroupArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationFsxLustre' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subdirectory', 'createLocationFsxLustre_subdirectory' - A subdirectory in the location\'s path. This subdirectory in the FSx for
-- Lustre file system is used to read data from the FSx for Lustre source
-- location or write data to the FSx for Lustre destination.
--
-- 'tags', 'createLocationFsxLustre_tags' - The key-value pair that represents a tag that you want to add to the
-- resource. The value can be an empty string. This value helps you manage,
-- filter, and search for your resources. We recommend that you create a
-- name tag for your location.
--
-- 'fsxFilesystemArn', 'createLocationFsxLustre_fsxFilesystemArn' - The Amazon Resource Name (ARN) for the FSx for Lustre file system.
--
-- 'securityGroupArns', 'createLocationFsxLustre_securityGroupArns' - The Amazon Resource Names (ARNs) of the security groups that are used to
-- configure the FSx for Lustre file system.
newCreateLocationFsxLustre ::
  -- | 'fsxFilesystemArn'
  Prelude.Text ->
  -- | 'securityGroupArns'
  Prelude.NonEmpty Prelude.Text ->
  CreateLocationFsxLustre
newCreateLocationFsxLustre
  pFsxFilesystemArn_
  pSecurityGroupArns_ =
    CreateLocationFsxLustre'
      { subdirectory =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        fsxFilesystemArn = pFsxFilesystemArn_,
        securityGroupArns =
          Lens.coerced Lens.# pSecurityGroupArns_
      }

-- | A subdirectory in the location\'s path. This subdirectory in the FSx for
-- Lustre file system is used to read data from the FSx for Lustre source
-- location or write data to the FSx for Lustre destination.
createLocationFsxLustre_subdirectory :: Lens.Lens' CreateLocationFsxLustre (Prelude.Maybe Prelude.Text)
createLocationFsxLustre_subdirectory = Lens.lens (\CreateLocationFsxLustre' {subdirectory} -> subdirectory) (\s@CreateLocationFsxLustre' {} a -> s {subdirectory = a} :: CreateLocationFsxLustre)

-- | The key-value pair that represents a tag that you want to add to the
-- resource. The value can be an empty string. This value helps you manage,
-- filter, and search for your resources. We recommend that you create a
-- name tag for your location.
createLocationFsxLustre_tags :: Lens.Lens' CreateLocationFsxLustre (Prelude.Maybe [TagListEntry])
createLocationFsxLustre_tags = Lens.lens (\CreateLocationFsxLustre' {tags} -> tags) (\s@CreateLocationFsxLustre' {} a -> s {tags = a} :: CreateLocationFsxLustre) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the FSx for Lustre file system.
createLocationFsxLustre_fsxFilesystemArn :: Lens.Lens' CreateLocationFsxLustre Prelude.Text
createLocationFsxLustre_fsxFilesystemArn = Lens.lens (\CreateLocationFsxLustre' {fsxFilesystemArn} -> fsxFilesystemArn) (\s@CreateLocationFsxLustre' {} a -> s {fsxFilesystemArn = a} :: CreateLocationFsxLustre)

-- | The Amazon Resource Names (ARNs) of the security groups that are used to
-- configure the FSx for Lustre file system.
createLocationFsxLustre_securityGroupArns :: Lens.Lens' CreateLocationFsxLustre (Prelude.NonEmpty Prelude.Text)
createLocationFsxLustre_securityGroupArns = Lens.lens (\CreateLocationFsxLustre' {securityGroupArns} -> securityGroupArns) (\s@CreateLocationFsxLustre' {} a -> s {securityGroupArns = a} :: CreateLocationFsxLustre) Prelude.. Lens.coerced

instance Core.AWSRequest CreateLocationFsxLustre where
  type
    AWSResponse CreateLocationFsxLustre =
      CreateLocationFsxLustreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationFsxLustreResponse'
            Prelude.<$> (x Data..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationFsxLustre where
  hashWithSalt _salt CreateLocationFsxLustre' {..} =
    _salt `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` fsxFilesystemArn
      `Prelude.hashWithSalt` securityGroupArns

instance Prelude.NFData CreateLocationFsxLustre where
  rnf CreateLocationFsxLustre' {..} =
    Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf fsxFilesystemArn
      `Prelude.seq` Prelude.rnf securityGroupArns

instance Data.ToHeaders CreateLocationFsxLustre where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.CreateLocationFsxLustre" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLocationFsxLustre where
  toJSON CreateLocationFsxLustre' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Subdirectory" Data..=) Prelude.<$> subdirectory,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("FsxFilesystemArn" Data..= fsxFilesystemArn),
            Prelude.Just
              ("SecurityGroupArns" Data..= securityGroupArns)
          ]
      )

instance Data.ToPath CreateLocationFsxLustre where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocationFsxLustre where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLocationFsxLustreResponse' smart constructor.
data CreateLocationFsxLustreResponse = CreateLocationFsxLustreResponse'
  { -- | The Amazon Resource Name (ARN) of the FSx for Lustre file system
    -- location that\'s created.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationFsxLustreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'createLocationFsxLustreResponse_locationArn' - The Amazon Resource Name (ARN) of the FSx for Lustre file system
-- location that\'s created.
--
-- 'httpStatus', 'createLocationFsxLustreResponse_httpStatus' - The response's http status code.
newCreateLocationFsxLustreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationFsxLustreResponse
newCreateLocationFsxLustreResponse pHttpStatus_ =
  CreateLocationFsxLustreResponse'
    { locationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the FSx for Lustre file system
-- location that\'s created.
createLocationFsxLustreResponse_locationArn :: Lens.Lens' CreateLocationFsxLustreResponse (Prelude.Maybe Prelude.Text)
createLocationFsxLustreResponse_locationArn = Lens.lens (\CreateLocationFsxLustreResponse' {locationArn} -> locationArn) (\s@CreateLocationFsxLustreResponse' {} a -> s {locationArn = a} :: CreateLocationFsxLustreResponse)

-- | The response's http status code.
createLocationFsxLustreResponse_httpStatus :: Lens.Lens' CreateLocationFsxLustreResponse Prelude.Int
createLocationFsxLustreResponse_httpStatus = Lens.lens (\CreateLocationFsxLustreResponse' {httpStatus} -> httpStatus) (\s@CreateLocationFsxLustreResponse' {} a -> s {httpStatus = a} :: CreateLocationFsxLustreResponse)

instance
  Prelude.NFData
    CreateLocationFsxLustreResponse
  where
  rnf CreateLocationFsxLustreResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
