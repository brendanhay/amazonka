{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.GitConfigForUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.GitConfigForUpdate
  ( GitConfigForUpdate (..),

    -- * Smart constructor
    mkGitConfigForUpdate,

    -- * Lenses
    gcfuSecretArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.SecretArn as Types

-- | Specifies configuration details for a Git repository when the repository is updated.
--
-- /See:/ 'mkGitConfigForUpdate' smart constructor.
newtype GitConfigForUpdate = GitConfigForUpdate'
  { -- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
    --
    -- @{"username": /UserName/ , "password": /Password/ }@
    secretArn :: Core.Maybe Types.SecretArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GitConfigForUpdate' value with any optional fields omitted.
mkGitConfigForUpdate ::
  GitConfigForUpdate
mkGitConfigForUpdate =
  GitConfigForUpdate' {secretArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@
--
-- /Note:/ Consider using 'secretArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfuSecretArn :: Lens.Lens' GitConfigForUpdate (Core.Maybe Types.SecretArn)
gcfuSecretArn = Lens.field @"secretArn"
{-# DEPRECATED gcfuSecretArn "Use generic-lens or generic-optics with 'secretArn' instead." #-}

instance Core.FromJSON GitConfigForUpdate where
  toJSON GitConfigForUpdate {..} =
    Core.object
      (Core.catMaybes [("SecretArn" Core..=) Core.<$> secretArn])
