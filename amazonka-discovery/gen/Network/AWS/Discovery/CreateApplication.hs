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
-- Module      : Network.AWS.Discovery.CreateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application with the given name and description.
module Network.AWS.Discovery.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_description,
    createApplication_name,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_configurationId,
    createApplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | Description of the application to be created.
    description :: Core.Maybe Core.Text,
    -- | Name of the application to be created.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createApplication_description' - Description of the application to be created.
--
-- 'name', 'createApplication_name' - Name of the application to be created.
newCreateApplication ::
  -- | 'name'
  Core.Text ->
  CreateApplication
newCreateApplication pName_ =
  CreateApplication'
    { description = Core.Nothing,
      name = pName_
    }

-- | Description of the application to be created.
createApplication_description :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_description = Lens.lens (\CreateApplication' {description} -> description) (\s@CreateApplication' {} a -> s {description = a} :: CreateApplication)

-- | Name of the application to be created.
createApplication_name :: Lens.Lens' CreateApplication Core.Text
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Core.<$> (x Core..?> "configurationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateApplication

instance Core.NFData CreateApplication

instance Core.ToHeaders CreateApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.CreateApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ ("description" Core..=) Core.<$> description,
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateApplication where
  toPath = Core.const "/"

instance Core.ToQuery CreateApplication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | Configuration ID of an application to be created.
    configurationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationId', 'createApplicationResponse_configurationId' - Configuration ID of an application to be created.
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateApplicationResponse
newCreateApplicationResponse pHttpStatus_ =
  CreateApplicationResponse'
    { configurationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Configuration ID of an application to be created.
createApplicationResponse_configurationId :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_configurationId = Lens.lens (\CreateApplicationResponse' {configurationId} -> configurationId) (\s@CreateApplicationResponse' {} a -> s {configurationId = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Core.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

instance Core.NFData CreateApplicationResponse
