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
-- Module      : Amazonka.AppConfig.CreateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application. In AppConfig, an application is simply an
-- organizational construct like a folder. This organizational construct
-- has a relationship with some unit of executable code. For example, you
-- could create an application called MyMobileApp to organize and manage
-- configuration data for a mobile application installed by your users.
module Amazonka.AppConfig.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_description,
    createApplication_tags,
    createApplication_name,

    -- * Destructuring the Response
    Application (..),
    newApplication,

    -- * Response Lenses
    application_description,
    application_id,
    application_name,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | A description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | Metadata to assign to the application. Tags help organize and categorize
    -- your AppConfig resources. Each tag consists of a key and an optional
    -- value, both of which you define.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A name for the application.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createApplication_description' - A description of the application.
--
-- 'tags', 'createApplication_tags' - Metadata to assign to the application. Tags help organize and categorize
-- your AppConfig resources. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- 'name', 'createApplication_name' - A name for the application.
newCreateApplication ::
  -- | 'name'
  Prelude.Text ->
  CreateApplication
newCreateApplication pName_ =
  CreateApplication'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | A description of the application.
createApplication_description :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_description = Lens.lens (\CreateApplication' {description} -> description) (\s@CreateApplication' {} a -> s {description = a} :: CreateApplication)

-- | Metadata to assign to the application. Tags help organize and categorize
-- your AppConfig resources. Each tag consists of a key and an optional
-- value, both of which you define.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | A name for the application.
createApplication_name :: Lens.Lens' CreateApplication Prelude.Text
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type AWSResponse CreateApplication = Application
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateApplication where
  toPath = Prelude.const "/applications"

instance Data.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty
