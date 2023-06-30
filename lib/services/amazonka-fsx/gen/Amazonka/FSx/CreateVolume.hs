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
-- Module      : Amazonka.FSx.CreateVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an FSx for ONTAP or Amazon FSx for OpenZFS storage volume.
module Amazonka.FSx.CreateVolume
  ( -- * Creating a Request
    CreateVolume (..),
    newCreateVolume,

    -- * Request Lenses
    createVolume_clientRequestToken,
    createVolume_ontapConfiguration,
    createVolume_openZFSConfiguration,
    createVolume_tags,
    createVolume_volumeType,
    createVolume_name,

    -- * Destructuring the Response
    CreateVolumeResponse (..),
    newCreateVolumeResponse,

    -- * Response Lenses
    createVolumeResponse_volume,
    createVolumeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVolume' smart constructor.
data CreateVolume = CreateVolume'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the configuration to use when creating the ONTAP volume.
    ontapConfiguration :: Prelude.Maybe CreateOntapVolumeConfiguration,
    -- | Specifies the configuration to use when creating the OpenZFS volume.
    openZFSConfiguration :: Prelude.Maybe CreateOpenZFSVolumeConfiguration,
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Specifies the type of volume to create; @ONTAP@ and @OPENZFS@ are the
    -- only valid volume types.
    volumeType :: VolumeType,
    -- | Specifies the name of the volume that you\'re creating.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createVolume_clientRequestToken' - Undocumented member.
--
-- 'ontapConfiguration', 'createVolume_ontapConfiguration' - Specifies the configuration to use when creating the ONTAP volume.
--
-- 'openZFSConfiguration', 'createVolume_openZFSConfiguration' - Specifies the configuration to use when creating the OpenZFS volume.
--
-- 'tags', 'createVolume_tags' - Undocumented member.
--
-- 'volumeType', 'createVolume_volumeType' - Specifies the type of volume to create; @ONTAP@ and @OPENZFS@ are the
-- only valid volume types.
--
-- 'name', 'createVolume_name' - Specifies the name of the volume that you\'re creating.
newCreateVolume ::
  -- | 'volumeType'
  VolumeType ->
  -- | 'name'
  Prelude.Text ->
  CreateVolume
newCreateVolume pVolumeType_ pName_ =
  CreateVolume'
    { clientRequestToken = Prelude.Nothing,
      ontapConfiguration = Prelude.Nothing,
      openZFSConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      volumeType = pVolumeType_,
      name = pName_
    }

-- | Undocumented member.
createVolume_clientRequestToken :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Text)
createVolume_clientRequestToken = Lens.lens (\CreateVolume' {clientRequestToken} -> clientRequestToken) (\s@CreateVolume' {} a -> s {clientRequestToken = a} :: CreateVolume)

-- | Specifies the configuration to use when creating the ONTAP volume.
createVolume_ontapConfiguration :: Lens.Lens' CreateVolume (Prelude.Maybe CreateOntapVolumeConfiguration)
createVolume_ontapConfiguration = Lens.lens (\CreateVolume' {ontapConfiguration} -> ontapConfiguration) (\s@CreateVolume' {} a -> s {ontapConfiguration = a} :: CreateVolume)

-- | Specifies the configuration to use when creating the OpenZFS volume.
createVolume_openZFSConfiguration :: Lens.Lens' CreateVolume (Prelude.Maybe CreateOpenZFSVolumeConfiguration)
createVolume_openZFSConfiguration = Lens.lens (\CreateVolume' {openZFSConfiguration} -> openZFSConfiguration) (\s@CreateVolume' {} a -> s {openZFSConfiguration = a} :: CreateVolume)

-- | Undocumented member.
createVolume_tags :: Lens.Lens' CreateVolume (Prelude.Maybe (Prelude.NonEmpty Tag))
createVolume_tags = Lens.lens (\CreateVolume' {tags} -> tags) (\s@CreateVolume' {} a -> s {tags = a} :: CreateVolume) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the type of volume to create; @ONTAP@ and @OPENZFS@ are the
-- only valid volume types.
createVolume_volumeType :: Lens.Lens' CreateVolume VolumeType
createVolume_volumeType = Lens.lens (\CreateVolume' {volumeType} -> volumeType) (\s@CreateVolume' {} a -> s {volumeType = a} :: CreateVolume)

-- | Specifies the name of the volume that you\'re creating.
createVolume_name :: Lens.Lens' CreateVolume Prelude.Text
createVolume_name = Lens.lens (\CreateVolume' {name} -> name) (\s@CreateVolume' {} a -> s {name = a} :: CreateVolume)

instance Core.AWSRequest CreateVolume where
  type AWSResponse CreateVolume = CreateVolumeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVolumeResponse'
            Prelude.<$> (x Data..?> "Volume")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVolume where
  hashWithSalt _salt CreateVolume' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` ontapConfiguration
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateVolume where
  rnf CreateVolume' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf ontapConfiguration
      `Prelude.seq` Prelude.rnf openZFSConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CreateVolume" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVolume where
  toJSON CreateVolume' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("OntapConfiguration" Data..=)
              Prelude.<$> ontapConfiguration,
            ("OpenZFSConfiguration" Data..=)
              Prelude.<$> openZFSConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("VolumeType" Data..= volumeType),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateVolume where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVolume where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVolumeResponse' smart constructor.
data CreateVolumeResponse = CreateVolumeResponse'
  { -- | Returned after a successful @CreateVolume@ API operation, describing the
    -- volume just created.
    volume :: Prelude.Maybe Volume,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volume', 'createVolumeResponse_volume' - Returned after a successful @CreateVolume@ API operation, describing the
-- volume just created.
--
-- 'httpStatus', 'createVolumeResponse_httpStatus' - The response's http status code.
newCreateVolumeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVolumeResponse
newCreateVolumeResponse pHttpStatus_ =
  CreateVolumeResponse'
    { volume = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returned after a successful @CreateVolume@ API operation, describing the
-- volume just created.
createVolumeResponse_volume :: Lens.Lens' CreateVolumeResponse (Prelude.Maybe Volume)
createVolumeResponse_volume = Lens.lens (\CreateVolumeResponse' {volume} -> volume) (\s@CreateVolumeResponse' {} a -> s {volume = a} :: CreateVolumeResponse)

-- | The response's http status code.
createVolumeResponse_httpStatus :: Lens.Lens' CreateVolumeResponse Prelude.Int
createVolumeResponse_httpStatus = Lens.lens (\CreateVolumeResponse' {httpStatus} -> httpStatus) (\s@CreateVolumeResponse' {} a -> s {httpStatus = a} :: CreateVolumeResponse)

instance Prelude.NFData CreateVolumeResponse where
  rnf CreateVolumeResponse' {..} =
    Prelude.rnf volume
      `Prelude.seq` Prelude.rnf httpStatus
