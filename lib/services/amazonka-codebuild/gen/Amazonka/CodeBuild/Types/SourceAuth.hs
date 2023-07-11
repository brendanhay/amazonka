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
-- Module      : Amazonka.CodeBuild.Types.SourceAuth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.SourceAuth where

import Amazonka.CodeBuild.Types.SourceAuthType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the authorization settings for CodeBuild to access the
-- source code to be built.
--
-- This information is for the CodeBuild console\'s use only. Your code
-- should not get or set this information directly.
--
-- /See:/ 'newSourceAuth' smart constructor.
data SourceAuth = SourceAuth'
  { -- | The resource value that applies to the specified authorization type.
    resource :: Prelude.Maybe Prelude.Text,
    -- | This data type is deprecated and is no longer accurate or used.
    --
    -- The authorization type to use. The only valid value is @OAUTH@, which
    -- represents the OAuth authorization type.
    type' :: SourceAuthType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceAuth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'sourceAuth_resource' - The resource value that applies to the specified authorization type.
--
-- 'type'', 'sourceAuth_type' - This data type is deprecated and is no longer accurate or used.
--
-- The authorization type to use. The only valid value is @OAUTH@, which
-- represents the OAuth authorization type.
newSourceAuth ::
  -- | 'type''
  SourceAuthType ->
  SourceAuth
newSourceAuth pType_ =
  SourceAuth'
    { resource = Prelude.Nothing,
      type' = pType_
    }

-- | The resource value that applies to the specified authorization type.
sourceAuth_resource :: Lens.Lens' SourceAuth (Prelude.Maybe Prelude.Text)
sourceAuth_resource = Lens.lens (\SourceAuth' {resource} -> resource) (\s@SourceAuth' {} a -> s {resource = a} :: SourceAuth)

-- | This data type is deprecated and is no longer accurate or used.
--
-- The authorization type to use. The only valid value is @OAUTH@, which
-- represents the OAuth authorization type.
sourceAuth_type :: Lens.Lens' SourceAuth SourceAuthType
sourceAuth_type = Lens.lens (\SourceAuth' {type'} -> type') (\s@SourceAuth' {} a -> s {type' = a} :: SourceAuth)

instance Data.FromJSON SourceAuth where
  parseJSON =
    Data.withObject
      "SourceAuth"
      ( \x ->
          SourceAuth'
            Prelude.<$> (x Data..:? "resource")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable SourceAuth where
  hashWithSalt _salt SourceAuth' {..} =
    _salt
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SourceAuth where
  rnf SourceAuth' {..} =
    Prelude.rnf resource
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON SourceAuth where
  toJSON SourceAuth' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resource" Data..=) Prelude.<$> resource,
            Prelude.Just ("type" Data..= type')
          ]
      )
