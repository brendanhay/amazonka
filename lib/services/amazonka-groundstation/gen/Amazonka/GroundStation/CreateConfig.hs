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
-- Module      : Amazonka.GroundStation.CreateConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Config@ with the specified @configData@ parameters.
--
-- Only one type of @configData@ can be specified.
module Amazonka.GroundStation.CreateConfig
  ( -- * Creating a Request
    CreateConfig (..),
    newCreateConfig,

    -- * Request Lenses
    createConfig_tags,
    createConfig_configData,
    createConfig_name,

    -- * Destructuring the Response
    ConfigIdResponse (..),
    newConfigIdResponse,

    -- * Response Lenses
    configIdResponse_configId,
    configIdResponse_configType,
    configIdResponse_configArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateConfig' smart constructor.
data CreateConfig = CreateConfig'
  { -- | Tags assigned to a @Config@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Parameters of a @Config@.
    configData :: ConfigTypeData,
    -- | Name of a @Config@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createConfig_tags' - Tags assigned to a @Config@.
--
-- 'configData', 'createConfig_configData' - Parameters of a @Config@.
--
-- 'name', 'createConfig_name' - Name of a @Config@.
newCreateConfig ::
  -- | 'configData'
  ConfigTypeData ->
  -- | 'name'
  Prelude.Text ->
  CreateConfig
newCreateConfig pConfigData_ pName_ =
  CreateConfig'
    { tags = Prelude.Nothing,
      configData = pConfigData_,
      name = pName_
    }

-- | Tags assigned to a @Config@.
createConfig_tags :: Lens.Lens' CreateConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConfig_tags = Lens.lens (\CreateConfig' {tags} -> tags) (\s@CreateConfig' {} a -> s {tags = a} :: CreateConfig) Prelude.. Lens.mapping Lens.coerced

-- | Parameters of a @Config@.
createConfig_configData :: Lens.Lens' CreateConfig ConfigTypeData
createConfig_configData = Lens.lens (\CreateConfig' {configData} -> configData) (\s@CreateConfig' {} a -> s {configData = a} :: CreateConfig)

-- | Name of a @Config@.
createConfig_name :: Lens.Lens' CreateConfig Prelude.Text
createConfig_name = Lens.lens (\CreateConfig' {name} -> name) (\s@CreateConfig' {} a -> s {name = a} :: CreateConfig)

instance Core.AWSRequest CreateConfig where
  type AWSResponse CreateConfig = ConfigIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateConfig where
  hashWithSalt _salt CreateConfig' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` configData
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateConfig where
  rnf CreateConfig' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf configData
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateConfig where
  toJSON CreateConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("configData" Core..= configData),
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateConfig where
  toPath = Prelude.const "/config"

instance Core.ToQuery CreateConfig where
  toQuery = Prelude.const Prelude.mempty
