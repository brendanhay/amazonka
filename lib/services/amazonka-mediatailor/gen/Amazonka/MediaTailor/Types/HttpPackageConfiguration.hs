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
-- Module      : Amazonka.MediaTailor.Types.HttpPackageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.HttpPackageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.Type
import qualified Amazonka.Prelude as Prelude

-- | The HTTP package configuration properties for the requested VOD source.
--
-- /See:/ 'newHttpPackageConfiguration' smart constructor.
data HttpPackageConfiguration = HttpPackageConfiguration'
  { -- | The relative path to the URL for this VOD source. This is combined with
    -- @SourceLocation::HttpConfiguration::BaseUrl@ to form a valid URL.
    path :: Prelude.Text,
    -- | The name of the source group. This has to match one of the
    -- @Channel::Outputs::SourceGroup@.
    sourceGroup :: Prelude.Text,
    -- | The streaming protocol for this package configuration. Supported values
    -- are @HLS@ and @DASH@.
    type' :: Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpPackageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'httpPackageConfiguration_path' - The relative path to the URL for this VOD source. This is combined with
-- @SourceLocation::HttpConfiguration::BaseUrl@ to form a valid URL.
--
-- 'sourceGroup', 'httpPackageConfiguration_sourceGroup' - The name of the source group. This has to match one of the
-- @Channel::Outputs::SourceGroup@.
--
-- 'type'', 'httpPackageConfiguration_type' - The streaming protocol for this package configuration. Supported values
-- are @HLS@ and @DASH@.
newHttpPackageConfiguration ::
  -- | 'path'
  Prelude.Text ->
  -- | 'sourceGroup'
  Prelude.Text ->
  -- | 'type''
  Type ->
  HttpPackageConfiguration
newHttpPackageConfiguration
  pPath_
  pSourceGroup_
  pType_ =
    HttpPackageConfiguration'
      { path = pPath_,
        sourceGroup = pSourceGroup_,
        type' = pType_
      }

-- | The relative path to the URL for this VOD source. This is combined with
-- @SourceLocation::HttpConfiguration::BaseUrl@ to form a valid URL.
httpPackageConfiguration_path :: Lens.Lens' HttpPackageConfiguration Prelude.Text
httpPackageConfiguration_path = Lens.lens (\HttpPackageConfiguration' {path} -> path) (\s@HttpPackageConfiguration' {} a -> s {path = a} :: HttpPackageConfiguration)

-- | The name of the source group. This has to match one of the
-- @Channel::Outputs::SourceGroup@.
httpPackageConfiguration_sourceGroup :: Lens.Lens' HttpPackageConfiguration Prelude.Text
httpPackageConfiguration_sourceGroup = Lens.lens (\HttpPackageConfiguration' {sourceGroup} -> sourceGroup) (\s@HttpPackageConfiguration' {} a -> s {sourceGroup = a} :: HttpPackageConfiguration)

-- | The streaming protocol for this package configuration. Supported values
-- are @HLS@ and @DASH@.
httpPackageConfiguration_type :: Lens.Lens' HttpPackageConfiguration Type
httpPackageConfiguration_type = Lens.lens (\HttpPackageConfiguration' {type'} -> type') (\s@HttpPackageConfiguration' {} a -> s {type' = a} :: HttpPackageConfiguration)

instance Data.FromJSON HttpPackageConfiguration where
  parseJSON =
    Data.withObject
      "HttpPackageConfiguration"
      ( \x ->
          HttpPackageConfiguration'
            Prelude.<$> (x Data..: "Path")
            Prelude.<*> (x Data..: "SourceGroup")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable HttpPackageConfiguration where
  hashWithSalt _salt HttpPackageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` sourceGroup
      `Prelude.hashWithSalt` type'

instance Prelude.NFData HttpPackageConfiguration where
  rnf HttpPackageConfiguration' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf sourceGroup
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON HttpPackageConfiguration where
  toJSON HttpPackageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Path" Data..= path),
            Prelude.Just ("SourceGroup" Data..= sourceGroup),
            Prelude.Just ("Type" Data..= type')
          ]
      )
