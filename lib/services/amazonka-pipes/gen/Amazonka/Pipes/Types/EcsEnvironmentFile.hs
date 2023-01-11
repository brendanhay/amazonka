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
-- Module      : Amazonka.Pipes.Types.EcsEnvironmentFile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.EcsEnvironmentFile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.EcsEnvironmentFileType
import qualified Amazonka.Prelude as Prelude

-- | A list of files containing the environment variables to pass to a
-- container. You can specify up to ten environment files. The file must
-- have a @.env@ file extension. Each line in an environment file should
-- contain an environment variable in @VARIABLE=VALUE@ format. Lines
-- beginning with @#@ are treated as comments and are ignored. For more
-- information about the environment variable file syntax, see
-- <https://docs.docker.com/compose/env-file/ Declare default environment variables in file>.
--
-- If there are environment variables specified using the @environment@
-- parameter in a container definition, they take precedence over the
-- variables contained within an environment file. If multiple environment
-- files are specified that contain the same variable, they\'re processed
-- from the top down. We recommend that you use unique variable names. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying environment variables>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- This parameter is only supported for tasks hosted on Fargate using the
-- following platform versions:
--
-- -   Linux platform version @1.4.0@ or later.
--
-- -   Windows platform version @1.0.0@ or later.
--
-- /See:/ 'newEcsEnvironmentFile' smart constructor.
data EcsEnvironmentFile = EcsEnvironmentFile'
  { -- | The file type to use. The only supported value is @s3@.
    type' :: EcsEnvironmentFileType,
    -- | The Amazon Resource Name (ARN) of the Amazon S3 object containing the
    -- environment variable file.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcsEnvironmentFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'ecsEnvironmentFile_type' - The file type to use. The only supported value is @s3@.
--
-- 'value', 'ecsEnvironmentFile_value' - The Amazon Resource Name (ARN) of the Amazon S3 object containing the
-- environment variable file.
newEcsEnvironmentFile ::
  -- | 'type''
  EcsEnvironmentFileType ->
  -- | 'value'
  Prelude.Text ->
  EcsEnvironmentFile
newEcsEnvironmentFile pType_ pValue_ =
  EcsEnvironmentFile'
    { type' = pType_,
      value = pValue_
    }

-- | The file type to use. The only supported value is @s3@.
ecsEnvironmentFile_type :: Lens.Lens' EcsEnvironmentFile EcsEnvironmentFileType
ecsEnvironmentFile_type = Lens.lens (\EcsEnvironmentFile' {type'} -> type') (\s@EcsEnvironmentFile' {} a -> s {type' = a} :: EcsEnvironmentFile)

-- | The Amazon Resource Name (ARN) of the Amazon S3 object containing the
-- environment variable file.
ecsEnvironmentFile_value :: Lens.Lens' EcsEnvironmentFile Prelude.Text
ecsEnvironmentFile_value = Lens.lens (\EcsEnvironmentFile' {value} -> value) (\s@EcsEnvironmentFile' {} a -> s {value = a} :: EcsEnvironmentFile)

instance Data.FromJSON EcsEnvironmentFile where
  parseJSON =
    Data.withObject
      "EcsEnvironmentFile"
      ( \x ->
          EcsEnvironmentFile'
            Prelude.<$> (x Data..: "type") Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable EcsEnvironmentFile where
  hashWithSalt _salt EcsEnvironmentFile' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData EcsEnvironmentFile where
  rnf EcsEnvironmentFile' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Data.ToJSON EcsEnvironmentFile where
  toJSON EcsEnvironmentFile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("type" Data..= type'),
            Prelude.Just ("value" Data..= value)
          ]
      )
