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
-- Module      : Amazonka.SageMaker.CreateSpace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a space used for real time collaboration in a Domain.
module Amazonka.SageMaker.CreateSpace
  ( -- * Creating a Request
    CreateSpace (..),
    newCreateSpace,

    -- * Request Lenses
    createSpace_spaceSettings,
    createSpace_tags,
    createSpace_domainId,
    createSpace_spaceName,

    -- * Destructuring the Response
    CreateSpaceResponse (..),
    newCreateSpaceResponse,

    -- * Response Lenses
    createSpaceResponse_spaceArn,
    createSpaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateSpace' smart constructor.
data CreateSpace = CreateSpace'
  { -- | A collection of space settings.
    spaceSettings :: Prelude.Maybe SpaceSettings,
    -- | Tags to associated with the space. Each tag consists of a key and an
    -- optional value. Tag keys must be unique for each resource. Tags are
    -- searchable using the @Search@ API.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the associated Domain.
    domainId :: Prelude.Text,
    -- | The name of the space.
    spaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSpace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spaceSettings', 'createSpace_spaceSettings' - A collection of space settings.
--
-- 'tags', 'createSpace_tags' - Tags to associated with the space. Each tag consists of a key and an
-- optional value. Tag keys must be unique for each resource. Tags are
-- searchable using the @Search@ API.
--
-- 'domainId', 'createSpace_domainId' - The ID of the associated Domain.
--
-- 'spaceName', 'createSpace_spaceName' - The name of the space.
newCreateSpace ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'spaceName'
  Prelude.Text ->
  CreateSpace
newCreateSpace pDomainId_ pSpaceName_ =
  CreateSpace'
    { spaceSettings = Prelude.Nothing,
      tags = Prelude.Nothing,
      domainId = pDomainId_,
      spaceName = pSpaceName_
    }

-- | A collection of space settings.
createSpace_spaceSettings :: Lens.Lens' CreateSpace (Prelude.Maybe SpaceSettings)
createSpace_spaceSettings = Lens.lens (\CreateSpace' {spaceSettings} -> spaceSettings) (\s@CreateSpace' {} a -> s {spaceSettings = a} :: CreateSpace)

-- | Tags to associated with the space. Each tag consists of a key and an
-- optional value. Tag keys must be unique for each resource. Tags are
-- searchable using the @Search@ API.
createSpace_tags :: Lens.Lens' CreateSpace (Prelude.Maybe [Tag])
createSpace_tags = Lens.lens (\CreateSpace' {tags} -> tags) (\s@CreateSpace' {} a -> s {tags = a} :: CreateSpace) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the associated Domain.
createSpace_domainId :: Lens.Lens' CreateSpace Prelude.Text
createSpace_domainId = Lens.lens (\CreateSpace' {domainId} -> domainId) (\s@CreateSpace' {} a -> s {domainId = a} :: CreateSpace)

-- | The name of the space.
createSpace_spaceName :: Lens.Lens' CreateSpace Prelude.Text
createSpace_spaceName = Lens.lens (\CreateSpace' {spaceName} -> spaceName) (\s@CreateSpace' {} a -> s {spaceName = a} :: CreateSpace)

instance Core.AWSRequest CreateSpace where
  type AWSResponse CreateSpace = CreateSpaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSpaceResponse'
            Prelude.<$> (x Data..?> "SpaceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSpace where
  hashWithSalt _salt CreateSpace' {..} =
    _salt
      `Prelude.hashWithSalt` spaceSettings
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` spaceName

instance Prelude.NFData CreateSpace where
  rnf CreateSpace' {..} =
    Prelude.rnf spaceSettings
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf spaceName

instance Data.ToHeaders CreateSpace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateSpace" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSpace where
  toJSON CreateSpace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SpaceSettings" Data..=) Prelude.<$> spaceSettings,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("SpaceName" Data..= spaceName)
          ]
      )

instance Data.ToPath CreateSpace where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSpace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSpaceResponse' smart constructor.
data CreateSpaceResponse = CreateSpaceResponse'
  { -- | The space\'s Amazon Resource Name (ARN).
    spaceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSpaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spaceArn', 'createSpaceResponse_spaceArn' - The space\'s Amazon Resource Name (ARN).
--
-- 'httpStatus', 'createSpaceResponse_httpStatus' - The response's http status code.
newCreateSpaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSpaceResponse
newCreateSpaceResponse pHttpStatus_ =
  CreateSpaceResponse'
    { spaceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The space\'s Amazon Resource Name (ARN).
createSpaceResponse_spaceArn :: Lens.Lens' CreateSpaceResponse (Prelude.Maybe Prelude.Text)
createSpaceResponse_spaceArn = Lens.lens (\CreateSpaceResponse' {spaceArn} -> spaceArn) (\s@CreateSpaceResponse' {} a -> s {spaceArn = a} :: CreateSpaceResponse)

-- | The response's http status code.
createSpaceResponse_httpStatus :: Lens.Lens' CreateSpaceResponse Prelude.Int
createSpaceResponse_httpStatus = Lens.lens (\CreateSpaceResponse' {httpStatus} -> httpStatus) (\s@CreateSpaceResponse' {} a -> s {httpStatus = a} :: CreateSpaceResponse)

instance Prelude.NFData CreateSpaceResponse where
  rnf CreateSpaceResponse' {..} =
    Prelude.rnf spaceArn
      `Prelude.seq` Prelude.rnf httpStatus
