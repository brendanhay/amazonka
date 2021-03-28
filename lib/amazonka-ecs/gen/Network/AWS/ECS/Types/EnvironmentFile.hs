{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EnvironmentFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.EnvironmentFile
  ( EnvironmentFile (..)
  -- * Smart constructor
  , mkEnvironmentFile
  -- * Lenses
  , efValue
  , efType
  ) where

import qualified Network.AWS.ECS.Types.EnvironmentFileType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of files containing the environment variables to pass to a container. You can specify up to ten environment files. The file must have a @.env@ file extension. Each line in an environment file should contain an environment variable in @VARIABLE=VALUE@ format. Lines beginning with @#@ are treated as comments and are ignored. For more information on the environment variable file syntax, see <https://docs.docker.com/compose/env-file/ Declare default environment variables in file> .
--
-- If there are environment variables specified using the @environment@ parameter in a container definition, they take precedence over the variables contained within an environment file. If multiple environment files are specified that contain the same variable, they are processed from the top down. It is recommended to use unique variable names. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/taskdef-envfiles.html Specifying Environment Variables> in the /Amazon Elastic Container Service Developer Guide/ .
-- This field is not valid for containers in tasks using the Fargate launch type.
--
-- /See:/ 'mkEnvironmentFile' smart constructor.
data EnvironmentFile = EnvironmentFile'
  { value :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Amazon S3 object containing the environment variable file.
  , type' :: Types.EnvironmentFileType
    -- ^ The file type to use. The only supported value is @s3@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentFile' value with any optional fields omitted.
mkEnvironmentFile
    :: Core.Text -- ^ 'value'
    -> Types.EnvironmentFileType -- ^ 'type\''
    -> EnvironmentFile
mkEnvironmentFile value type' = EnvironmentFile'{value, type'}

-- | The Amazon Resource Name (ARN) of the Amazon S3 object containing the environment variable file.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efValue :: Lens.Lens' EnvironmentFile Core.Text
efValue = Lens.field @"value"
{-# INLINEABLE efValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | The file type to use. The only supported value is @s3@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efType :: Lens.Lens' EnvironmentFile Types.EnvironmentFileType
efType = Lens.field @"type'"
{-# INLINEABLE efType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON EnvironmentFile where
        toJSON EnvironmentFile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("value" Core..= value),
                  Core.Just ("type" Core..= type')])

instance Core.FromJSON EnvironmentFile where
        parseJSON
          = Core.withObject "EnvironmentFile" Core.$
              \ x ->
                EnvironmentFile' Core.<$>
                  (x Core..: "value") Core.<*> x Core..: "type"
