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
-- Module      : Network.AWS.ECS.Types.EnvironmentFile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EnvironmentFile where

import Network.AWS.ECS.Types.EnvironmentFileType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of files containing the environment variables to pass to a
-- container. You can specify up to ten environment files. The file must
-- have a @.env@ file extension. Each line in an environment file should
-- contain an environment variable in @VARIABLE=VALUE@ format. Lines
-- beginning with @#@ are treated as comments and are ignored. For more
-- information on the environment variable file syntax, see
-- <https://docs.docker.com/compose/env-file/ Declare default environment variables in file>.
--
-- If there are environment variables specified using the @environment@
-- parameter in a container definition, they take precedence over the
-- variables contained within an environment file. If multiple environment
-- files are specified that contain the same variable, they are processed
-- from the top down. It is recommended to use unique variable names. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- This field is not valid for containers in tasks using the Fargate launch
-- type.
--
-- /See:/ 'newEnvironmentFile' smart constructor.
data EnvironmentFile = EnvironmentFile'
  { -- | The Amazon Resource Name (ARN) of the Amazon S3 object containing the
    -- environment variable file.
    value :: Prelude.Text,
    -- | The file type to use. The only supported value is @s3@.
    type' :: EnvironmentFileType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'environmentFile_value' - The Amazon Resource Name (ARN) of the Amazon S3 object containing the
-- environment variable file.
--
-- 'type'', 'environmentFile_type' - The file type to use. The only supported value is @s3@.
newEnvironmentFile ::
  -- | 'value'
  Prelude.Text ->
  -- | 'type''
  EnvironmentFileType ->
  EnvironmentFile
newEnvironmentFile pValue_ pType_ =
  EnvironmentFile' {value = pValue_, type' = pType_}

-- | The Amazon Resource Name (ARN) of the Amazon S3 object containing the
-- environment variable file.
environmentFile_value :: Lens.Lens' EnvironmentFile Prelude.Text
environmentFile_value = Lens.lens (\EnvironmentFile' {value} -> value) (\s@EnvironmentFile' {} a -> s {value = a} :: EnvironmentFile)

-- | The file type to use. The only supported value is @s3@.
environmentFile_type :: Lens.Lens' EnvironmentFile EnvironmentFileType
environmentFile_type = Lens.lens (\EnvironmentFile' {type'} -> type') (\s@EnvironmentFile' {} a -> s {type' = a} :: EnvironmentFile)

instance Prelude.FromJSON EnvironmentFile where
  parseJSON =
    Prelude.withObject
      "EnvironmentFile"
      ( \x ->
          EnvironmentFile'
            Prelude.<$> (x Prelude..: "value")
            Prelude.<*> (x Prelude..: "type")
      )

instance Prelude.Hashable EnvironmentFile

instance Prelude.NFData EnvironmentFile

instance Prelude.ToJSON EnvironmentFile where
  toJSON EnvironmentFile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("value" Prelude..= value),
            Prelude.Just ("type" Prelude..= type')
          ]
      )
