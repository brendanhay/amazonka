{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.DevEndpointCustomLibraries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DevEndpointCustomLibraries where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Custom libraries to be loaded into a development endpoint.
--
-- /See:/ 'newDevEndpointCustomLibraries' smart constructor.
data DevEndpointCustomLibraries = DevEndpointCustomLibraries'
  { -- | The paths to one or more Python libraries in an Amazon Simple Storage
    -- Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@.
    -- Multiple values must be complete paths separated by a comma.
    --
    -- You can only use pure Python libraries with a @DevEndpoint@. Libraries
    -- that rely on C extensions, such as the
    -- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
    -- currently supported.
    extraPythonLibsS3Path :: Prelude.Maybe Prelude.Text,
    -- | The path to one or more Java @.jar@ files in an S3 bucket that should be
    -- loaded in your @DevEndpoint@.
    --
    -- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
    extraJarsS3Path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DevEndpointCustomLibraries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extraPythonLibsS3Path', 'devEndpointCustomLibraries_extraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon Simple Storage
-- Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@.
-- Multiple values must be complete paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- currently supported.
--
-- 'extraJarsS3Path', 'devEndpointCustomLibraries_extraJarsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
--
-- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
newDevEndpointCustomLibraries ::
  DevEndpointCustomLibraries
newDevEndpointCustomLibraries =
  DevEndpointCustomLibraries'
    { extraPythonLibsS3Path =
        Prelude.Nothing,
      extraJarsS3Path = Prelude.Nothing
    }

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

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
--
-- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
devEndpointCustomLibraries_extraJarsS3Path :: Lens.Lens' DevEndpointCustomLibraries (Prelude.Maybe Prelude.Text)
devEndpointCustomLibraries_extraJarsS3Path = Lens.lens (\DevEndpointCustomLibraries' {extraJarsS3Path} -> extraJarsS3Path) (\s@DevEndpointCustomLibraries' {} a -> s {extraJarsS3Path = a} :: DevEndpointCustomLibraries)

instance Prelude.Hashable DevEndpointCustomLibraries

instance Prelude.NFData DevEndpointCustomLibraries

instance Prelude.ToJSON DevEndpointCustomLibraries where
  toJSON DevEndpointCustomLibraries' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExtraPythonLibsS3Path" Prelude..=)
              Prelude.<$> extraPythonLibsS3Path,
            ("ExtraJarsS3Path" Prelude..=)
              Prelude.<$> extraJarsS3Path
          ]
      )
