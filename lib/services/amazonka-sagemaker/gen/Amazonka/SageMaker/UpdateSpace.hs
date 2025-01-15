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
-- Module      : Amazonka.SageMaker.UpdateSpace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a space.
module Amazonka.SageMaker.UpdateSpace
  ( -- * Creating a Request
    UpdateSpace (..),
    newUpdateSpace,

    -- * Request Lenses
    updateSpace_spaceSettings,
    updateSpace_domainId,
    updateSpace_spaceName,

    -- * Destructuring the Response
    UpdateSpaceResponse (..),
    newUpdateSpaceResponse,

    -- * Response Lenses
    updateSpaceResponse_spaceArn,
    updateSpaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateSpace' smart constructor.
data UpdateSpace = UpdateSpace'
  { -- | A collection of space settings.
    spaceSettings :: Prelude.Maybe SpaceSettings,
    -- | The ID of the associated Domain.
    domainId :: Prelude.Text,
    -- | The name of the space.
    spaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSpace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spaceSettings', 'updateSpace_spaceSettings' - A collection of space settings.
--
-- 'domainId', 'updateSpace_domainId' - The ID of the associated Domain.
--
-- 'spaceName', 'updateSpace_spaceName' - The name of the space.
newUpdateSpace ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'spaceName'
  Prelude.Text ->
  UpdateSpace
newUpdateSpace pDomainId_ pSpaceName_ =
  UpdateSpace'
    { spaceSettings = Prelude.Nothing,
      domainId = pDomainId_,
      spaceName = pSpaceName_
    }

-- | A collection of space settings.
updateSpace_spaceSettings :: Lens.Lens' UpdateSpace (Prelude.Maybe SpaceSettings)
updateSpace_spaceSettings = Lens.lens (\UpdateSpace' {spaceSettings} -> spaceSettings) (\s@UpdateSpace' {} a -> s {spaceSettings = a} :: UpdateSpace)

-- | The ID of the associated Domain.
updateSpace_domainId :: Lens.Lens' UpdateSpace Prelude.Text
updateSpace_domainId = Lens.lens (\UpdateSpace' {domainId} -> domainId) (\s@UpdateSpace' {} a -> s {domainId = a} :: UpdateSpace)

-- | The name of the space.
updateSpace_spaceName :: Lens.Lens' UpdateSpace Prelude.Text
updateSpace_spaceName = Lens.lens (\UpdateSpace' {spaceName} -> spaceName) (\s@UpdateSpace' {} a -> s {spaceName = a} :: UpdateSpace)

instance Core.AWSRequest UpdateSpace where
  type AWSResponse UpdateSpace = UpdateSpaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSpaceResponse'
            Prelude.<$> (x Data..?> "SpaceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSpace where
  hashWithSalt _salt UpdateSpace' {..} =
    _salt
      `Prelude.hashWithSalt` spaceSettings
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` spaceName

instance Prelude.NFData UpdateSpace where
  rnf UpdateSpace' {..} =
    Prelude.rnf spaceSettings `Prelude.seq`
      Prelude.rnf domainId `Prelude.seq`
        Prelude.rnf spaceName

instance Data.ToHeaders UpdateSpace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdateSpace" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSpace where
  toJSON UpdateSpace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SpaceSettings" Data..=) Prelude.<$> spaceSettings,
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("SpaceName" Data..= spaceName)
          ]
      )

instance Data.ToPath UpdateSpace where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSpace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSpaceResponse' smart constructor.
data UpdateSpaceResponse = UpdateSpaceResponse'
  { -- | The space\'s Amazon Resource Name (ARN).
    spaceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSpaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spaceArn', 'updateSpaceResponse_spaceArn' - The space\'s Amazon Resource Name (ARN).
--
-- 'httpStatus', 'updateSpaceResponse_httpStatus' - The response's http status code.
newUpdateSpaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSpaceResponse
newUpdateSpaceResponse pHttpStatus_ =
  UpdateSpaceResponse'
    { spaceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The space\'s Amazon Resource Name (ARN).
updateSpaceResponse_spaceArn :: Lens.Lens' UpdateSpaceResponse (Prelude.Maybe Prelude.Text)
updateSpaceResponse_spaceArn = Lens.lens (\UpdateSpaceResponse' {spaceArn} -> spaceArn) (\s@UpdateSpaceResponse' {} a -> s {spaceArn = a} :: UpdateSpaceResponse)

-- | The response's http status code.
updateSpaceResponse_httpStatus :: Lens.Lens' UpdateSpaceResponse Prelude.Int
updateSpaceResponse_httpStatus = Lens.lens (\UpdateSpaceResponse' {httpStatus} -> httpStatus) (\s@UpdateSpaceResponse' {} a -> s {httpStatus = a} :: UpdateSpaceResponse)

instance Prelude.NFData UpdateSpaceResponse where
  rnf UpdateSpaceResponse' {..} =
    Prelude.rnf spaceArn `Prelude.seq`
      Prelude.rnf httpStatus
