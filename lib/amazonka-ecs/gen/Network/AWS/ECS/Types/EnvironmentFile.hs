{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EnvironmentFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EnvironmentFile
  ( EnvironmentFile (..),

    -- * Smart constructor
    mkEnvironmentFile,

    -- * Lenses
    efValue,
    efType,
  )
where

import Network.AWS.ECS.Types.EnvironmentFileType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of files containing the environment variables to pass to a container. You can specify up to ten environment files. The file must have a @.env@ file extension. Each line in an environment file should contain an environment variable in @VARIABLE=VALUE@ format. Lines beginning with @#@ are treated as comments and are ignored. For more information on the environment variable file syntax, see <https://docs.docker.com/compose/env-file/ Declare default environment variables in file> .
--
-- If there are environment variables specified using the @environment@ parameter in a container definition, they take precedence over the variables contained within an environment file. If multiple environment files are specified that contain the same variable, they are processed from the top down. It is recommended to use unique variable names. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables> in the /Amazon Elastic Container Service Developer Guide/ .
-- This field is not valid for containers in tasks using the Fargate launch type.
--
-- /See:/ 'mkEnvironmentFile' smart constructor.
data EnvironmentFile = EnvironmentFile'
  { value :: Lude.Text,
    type' :: EnvironmentFileType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentFile' with the minimum fields required to make a request.
--
-- * 'type'' - The file type to use. The only supported value is @s3@ .
-- * 'value' - The Amazon Resource Name (ARN) of the Amazon S3 object containing the environment variable file.
mkEnvironmentFile ::
  -- | 'value'
  Lude.Text ->
  -- | 'type''
  EnvironmentFileType ->
  EnvironmentFile
mkEnvironmentFile pValue_ pType_ =
  EnvironmentFile' {value = pValue_, type' = pType_}

-- | The Amazon Resource Name (ARN) of the Amazon S3 object containing the environment variable file.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efValue :: Lens.Lens' EnvironmentFile Lude.Text
efValue = Lens.lens (value :: EnvironmentFile -> Lude.Text) (\s a -> s {value = a} :: EnvironmentFile)
{-# DEPRECATED efValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The file type to use. The only supported value is @s3@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efType :: Lens.Lens' EnvironmentFile EnvironmentFileType
efType = Lens.lens (type' :: EnvironmentFile -> EnvironmentFileType) (\s a -> s {type' = a} :: EnvironmentFile)
{-# DEPRECATED efType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON EnvironmentFile where
  parseJSON =
    Lude.withObject
      "EnvironmentFile"
      ( \x ->
          EnvironmentFile'
            Lude.<$> (x Lude..: "value") Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON EnvironmentFile where
  toJSON EnvironmentFile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("value" Lude..= value),
            Lude.Just ("type" Lude..= type')
          ]
      )
