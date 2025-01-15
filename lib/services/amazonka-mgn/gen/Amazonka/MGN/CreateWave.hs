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
-- Module      : Amazonka.MGN.CreateWave
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create wave.
module Amazonka.MGN.CreateWave
  ( -- * Creating a Request
    CreateWave (..),
    newCreateWave,

    -- * Request Lenses
    createWave_description,
    createWave_tags,
    createWave_name,

    -- * Destructuring the Response
    Wave (..),
    newWave,

    -- * Response Lenses
    wave_arn,
    wave_creationDateTime,
    wave_description,
    wave_isArchived,
    wave_lastModifiedDateTime,
    wave_name,
    wave_tags,
    wave_waveAggregatedStatus,
    wave_waveID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWave' smart constructor.
data CreateWave = CreateWave'
  { -- | Wave description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Wave tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Wave name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWave' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createWave_description' - Wave description.
--
-- 'tags', 'createWave_tags' - Wave tags.
--
-- 'name', 'createWave_name' - Wave name.
newCreateWave ::
  -- | 'name'
  Prelude.Text ->
  CreateWave
newCreateWave pName_ =
  CreateWave'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | Wave description.
createWave_description :: Lens.Lens' CreateWave (Prelude.Maybe Prelude.Text)
createWave_description = Lens.lens (\CreateWave' {description} -> description) (\s@CreateWave' {} a -> s {description = a} :: CreateWave)

-- | Wave tags.
createWave_tags :: Lens.Lens' CreateWave (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWave_tags = Lens.lens (\CreateWave' {tags} -> tags) (\s@CreateWave' {} a -> s {tags = a} :: CreateWave) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Wave name.
createWave_name :: Lens.Lens' CreateWave Prelude.Text
createWave_name = Lens.lens (\CreateWave' {name} -> name) (\s@CreateWave' {} a -> s {name = a} :: CreateWave)

instance Core.AWSRequest CreateWave where
  type AWSResponse CreateWave = Wave
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateWave where
  hashWithSalt _salt CreateWave' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateWave where
  rnf CreateWave' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf name

instance Data.ToHeaders CreateWave where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWave where
  toJSON CreateWave' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateWave where
  toPath = Prelude.const "/CreateWave"

instance Data.ToQuery CreateWave where
  toQuery = Prelude.const Prelude.mempty
