{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types.CreateApplicationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CreateApplicationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the display name of an application and the tags to associate
-- with the application.
--
-- /See:/ 'newCreateApplicationRequest' smart constructor.
data CreateApplicationRequest = CreateApplicationRequest'
  { -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the application. Each tag consists of a required tag key
    -- and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The display name of the application. This name is displayed as the
    -- __Project name__ on the Amazon Pinpoint console.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createApplicationRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the application. Each tag consists of a required tag key
-- and an associated tag value.
--
-- 'name', 'createApplicationRequest_name' - The display name of the application. This name is displayed as the
-- __Project name__ on the Amazon Pinpoint console.
newCreateApplicationRequest ::
  -- | 'name'
  Prelude.Text ->
  CreateApplicationRequest
newCreateApplicationRequest pName_ =
  CreateApplicationRequest'
    { tags = Prelude.Nothing,
      name = pName_
    }

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the application. Each tag consists of a required tag key
-- and an associated tag value.
createApplicationRequest_tags :: Lens.Lens' CreateApplicationRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplicationRequest_tags = Lens.lens (\CreateApplicationRequest' {tags} -> tags) (\s@CreateApplicationRequest' {} a -> s {tags = a} :: CreateApplicationRequest) Prelude.. Lens.mapping Lens.coerced

-- | The display name of the application. This name is displayed as the
-- __Project name__ on the Amazon Pinpoint console.
createApplicationRequest_name :: Lens.Lens' CreateApplicationRequest Prelude.Text
createApplicationRequest_name = Lens.lens (\CreateApplicationRequest' {name} -> name) (\s@CreateApplicationRequest' {} a -> s {name = a} :: CreateApplicationRequest)

instance Prelude.Hashable CreateApplicationRequest where
  hashWithSalt _salt CreateApplicationRequest' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateApplicationRequest where
  rnf CreateApplicationRequest' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf name

instance Data.ToJSON CreateApplicationRequest where
  toJSON CreateApplicationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )
