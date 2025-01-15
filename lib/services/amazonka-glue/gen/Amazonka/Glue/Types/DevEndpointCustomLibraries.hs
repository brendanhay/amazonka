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
-- Module      : Amazonka.Glue.Types.DevEndpointCustomLibraries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DevEndpointCustomLibraries where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Custom libraries to be loaded into a development endpoint.
--
-- /See:/ 'newDevEndpointCustomLibraries' smart constructor.
data DevEndpointCustomLibraries = DevEndpointCustomLibraries'
  { -- | The path to one or more Java @.jar@ files in an S3 bucket that should be
    -- loaded in your @DevEndpoint@.
    --
    -- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
    extraJarsS3Path :: Prelude.Maybe Prelude.Text,
    -- | The paths to one or more Python libraries in an Amazon Simple Storage
    -- Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@.
    -- Multiple values must be complete paths separated by a comma.
    --
    -- You can only use pure Python libraries with a @DevEndpoint@. Libraries
    -- that rely on C extensions, such as the
    -- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
    -- currently supported.
    extraPythonLibsS3Path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DevEndpointCustomLibraries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extraJarsS3Path', 'devEndpointCustomLibraries_extraJarsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
--
-- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
--
-- 'extraPythonLibsS3Path', 'devEndpointCustomLibraries_extraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon Simple Storage
-- Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@.
-- Multiple values must be complete paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- currently supported.
newDevEndpointCustomLibraries ::
  DevEndpointCustomLibraries
newDevEndpointCustomLibraries =
  DevEndpointCustomLibraries'
    { extraJarsS3Path =
        Prelude.Nothing,
      extraPythonLibsS3Path = Prelude.Nothing
    }

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
--
-- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
devEndpointCustomLibraries_extraJarsS3Path :: Lens.Lens' DevEndpointCustomLibraries (Prelude.Maybe Prelude.Text)
devEndpointCustomLibraries_extraJarsS3Path = Lens.lens (\DevEndpointCustomLibraries' {extraJarsS3Path} -> extraJarsS3Path) (\s@DevEndpointCustomLibraries' {} a -> s {extraJarsS3Path = a} :: DevEndpointCustomLibraries)

-- | The paths to one or more Python libraries in an Amazon Simple Storage
-- Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@.
-- Multiple values must be complete paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- currently supported.
devEndpointCustomLibraries_extraPythonLibsS3Path :: Lens.Lens' DevEndpointCustomLibraries (Prelude.Maybe Prelude.Text)
devEndpointCustomLibraries_extraPythonLibsS3Path = Lens.lens (\DevEndpointCustomLibraries' {extraPythonLibsS3Path} -> extraPythonLibsS3Path) (\s@DevEndpointCustomLibraries' {} a -> s {extraPythonLibsS3Path = a} :: DevEndpointCustomLibraries)

instance Prelude.Hashable DevEndpointCustomLibraries where
  hashWithSalt _salt DevEndpointCustomLibraries' {..} =
    _salt
      `Prelude.hashWithSalt` extraJarsS3Path
      `Prelude.hashWithSalt` extraPythonLibsS3Path

instance Prelude.NFData DevEndpointCustomLibraries where
  rnf DevEndpointCustomLibraries' {..} =
    Prelude.rnf extraJarsS3Path `Prelude.seq`
      Prelude.rnf extraPythonLibsS3Path

instance Data.ToJSON DevEndpointCustomLibraries where
  toJSON DevEndpointCustomLibraries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExtraJarsS3Path" Data..=)
              Prelude.<$> extraJarsS3Path,
            ("ExtraPythonLibsS3Path" Data..=)
              Prelude.<$> extraPythonLibsS3Path
          ]
      )
