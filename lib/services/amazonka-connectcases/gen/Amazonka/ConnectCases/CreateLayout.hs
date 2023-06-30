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
-- Module      : Amazonka.ConnectCases.CreateLayout
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a layout in the Cases domain. Layouts define the following
-- configuration in the top section and More Info tab of the Cases user
-- interface:
--
-- -   Fields to display to the users
--
-- -   Field ordering
--
-- Title and Status fields cannot be part of layouts since they are not
-- configurable.
module Amazonka.ConnectCases.CreateLayout
  ( -- * Creating a Request
    CreateLayout (..),
    newCreateLayout,

    -- * Request Lenses
    createLayout_content,
    createLayout_domainId,
    createLayout_name,

    -- * Destructuring the Response
    CreateLayoutResponse (..),
    newCreateLayoutResponse,

    -- * Response Lenses
    createLayoutResponse_httpStatus,
    createLayoutResponse_layoutArn,
    createLayoutResponse_layoutId,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLayout' smart constructor.
data CreateLayout = CreateLayout'
  { -- | Information about which fields will be present in the layout, and
    -- information about the order of the fields.
    content :: LayoutContent,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | The name of the layout. It must be unique for the Cases domain.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLayout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'createLayout_content' - Information about which fields will be present in the layout, and
-- information about the order of the fields.
--
-- 'domainId', 'createLayout_domainId' - The unique identifier of the Cases domain.
--
-- 'name', 'createLayout_name' - The name of the layout. It must be unique for the Cases domain.
newCreateLayout ::
  -- | 'content'
  LayoutContent ->
  -- | 'domainId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateLayout
newCreateLayout pContent_ pDomainId_ pName_ =
  CreateLayout'
    { content = pContent_,
      domainId = pDomainId_,
      name = pName_
    }

-- | Information about which fields will be present in the layout, and
-- information about the order of the fields.
createLayout_content :: Lens.Lens' CreateLayout LayoutContent
createLayout_content = Lens.lens (\CreateLayout' {content} -> content) (\s@CreateLayout' {} a -> s {content = a} :: CreateLayout)

-- | The unique identifier of the Cases domain.
createLayout_domainId :: Lens.Lens' CreateLayout Prelude.Text
createLayout_domainId = Lens.lens (\CreateLayout' {domainId} -> domainId) (\s@CreateLayout' {} a -> s {domainId = a} :: CreateLayout)

-- | The name of the layout. It must be unique for the Cases domain.
createLayout_name :: Lens.Lens' CreateLayout Prelude.Text
createLayout_name = Lens.lens (\CreateLayout' {name} -> name) (\s@CreateLayout' {} a -> s {name = a} :: CreateLayout)

instance Core.AWSRequest CreateLayout where
  type AWSResponse CreateLayout = CreateLayoutResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLayoutResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "layoutArn")
            Prelude.<*> (x Data..:> "layoutId")
      )

instance Prelude.Hashable CreateLayout where
  hashWithSalt _salt CreateLayout' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateLayout where
  rnf CreateLayout' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateLayout where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLayout where
  toJSON CreateLayout' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("content" Data..= content),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateLayout where
  toPath CreateLayout' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainId, "/layouts"]

instance Data.ToQuery CreateLayout where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLayoutResponse' smart constructor.
data CreateLayoutResponse = CreateLayoutResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the newly created layout.
    layoutArn :: Prelude.Text,
    -- | The unique identifier of the layout.
    layoutId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLayoutResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLayoutResponse_httpStatus' - The response's http status code.
--
-- 'layoutArn', 'createLayoutResponse_layoutArn' - The Amazon Resource Name (ARN) of the newly created layout.
--
-- 'layoutId', 'createLayoutResponse_layoutId' - The unique identifier of the layout.
newCreateLayoutResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'layoutArn'
  Prelude.Text ->
  -- | 'layoutId'
  Prelude.Text ->
  CreateLayoutResponse
newCreateLayoutResponse
  pHttpStatus_
  pLayoutArn_
  pLayoutId_ =
    CreateLayoutResponse'
      { httpStatus = pHttpStatus_,
        layoutArn = pLayoutArn_,
        layoutId = pLayoutId_
      }

-- | The response's http status code.
createLayoutResponse_httpStatus :: Lens.Lens' CreateLayoutResponse Prelude.Int
createLayoutResponse_httpStatus = Lens.lens (\CreateLayoutResponse' {httpStatus} -> httpStatus) (\s@CreateLayoutResponse' {} a -> s {httpStatus = a} :: CreateLayoutResponse)

-- | The Amazon Resource Name (ARN) of the newly created layout.
createLayoutResponse_layoutArn :: Lens.Lens' CreateLayoutResponse Prelude.Text
createLayoutResponse_layoutArn = Lens.lens (\CreateLayoutResponse' {layoutArn} -> layoutArn) (\s@CreateLayoutResponse' {} a -> s {layoutArn = a} :: CreateLayoutResponse)

-- | The unique identifier of the layout.
createLayoutResponse_layoutId :: Lens.Lens' CreateLayoutResponse Prelude.Text
createLayoutResponse_layoutId = Lens.lens (\CreateLayoutResponse' {layoutId} -> layoutId) (\s@CreateLayoutResponse' {} a -> s {layoutId = a} :: CreateLayoutResponse)

instance Prelude.NFData CreateLayoutResponse where
  rnf CreateLayoutResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf layoutArn
      `Prelude.seq` Prelude.rnf layoutId
